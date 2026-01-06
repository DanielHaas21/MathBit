{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Cleanup (cleanupExpr) where

import Struct.Rule
import Struct.Expr
import Struct.Step
import Helpers.FixPoint (fixpoint)
import Helpers.Numbers (negNum, numberLtZero, isOne, isZero)

cleanupExpr :: Expr -> Expr
cleanupExpr = fixpoint cleanupOnce

cleanupOnce :: Expr -> Expr
cleanupOnce =
    cleanupNegPowers
  . cleanupPowPow
  . cleanupPowOne
  . cleanupNegOneOnVar
  . mapChildren cleanupOnce

mapChildren :: (Expr -> Expr) -> Expr -> Expr
mapChildren f = \case
  Add a b -> Add (f a) (f b)
  Mul a b -> Mul (f a) (f b)
  Div a b -> Div (f a) (f b)
  Pow a b -> Pow (f a) (f b)

  Func fn a -> Func fn (f a)
  Sqrt a    -> Sqrt (f a)
  Root n a  -> Root (f n) (f a)

  e -> e

-- ====================
-- Cleanup negative powers
-- ====================
cleanupNegPowers :: Expr -> Expr
cleanupNegPowers = \case
  Mul a (Pow x (Num n)) | numberLtZero n ->
    Div a (Pow x (Num (negNum n)))

  Mul (Pow x (Num n)) a | numberLtZero n ->
    Div a (Pow x (Num (negNum n)))

  e -> e

-- ====================
-- Pow 1 simplification
-- ====================
cleanupPowOne :: Expr -> Expr
cleanupPowOne = \case
  Pow a (Num n) | isOne n -> a
  e -> e

-- ====================
-- Flatten nested powers
-- ====================
cleanupPowPow :: Expr -> Expr
cleanupPowPow = \case
  Pow (Pow a b) c -> Pow a (Mul b c)
  e -> e

cleanupNegOneOnVar :: Expr -> Expr
cleanupNegOneOnVar = \case
  Mul (Num n) x@(Var _) | isNegOne n -> Neg x
  e -> e

-- Helper to detect exactly -1
isNegOne :: Number -> Bool
isNegOne (R r) = r == -1
isNegOne (D d) = d == -1