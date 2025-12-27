{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Cleanup (cleanupExpr) where

import Struct.Rule
import Struct.Expr
import Struct.Step

cleanupExpr :: Expr -> Expr
cleanupExpr =
  fixpoint cleanupOnce

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
  in if x' == x then x else fixpoint f x'

cleanupOnce :: Expr -> Expr
cleanupOnce =
  cleanupNegPowers
  . cleanupPowPow
  . mapChildren cleanupOnce

mapChildren :: (Expr -> Expr) -> Expr -> Expr
mapChildren f = \case
  Add a b -> Add (f a) (f b)
  Mul a b -> Mul (f a) (f b)
  Pow a b -> Pow (f a) (f b)

  Func fn a -> Func fn (f a)
  Sqrt a    -> Sqrt (f a)
  Root n a  -> Root (f n) (f a)

  e -> e

cleanupNegPowers :: Expr -> Expr
cleanupNegPowers = \case
  Mul a (Pow x (Num n)) | n < 0 ->
    Div a (Pow x (Num (-n)))

  Mul (Pow x (Num n)) a | n < 0 ->
    Div a (Pow x (Num (-n)))

  e -> e

cleanupPowPow :: Expr -> Expr
cleanupPowPow = \case
  Pow (Pow a b) c ->
    Pow a (Mul b c)

  e -> e