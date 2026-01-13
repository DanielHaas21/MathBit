{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Cleanup (cleanupExpr) where

import Struct.Rule
import Struct.Expr
import Struct.Step
import Helpers.FixPoint (fixpoint)
import Helpers.Numbers (negNum, numberLtZero, isOne, isZero)

-- This module performs various cleanup operations on expressions after normalization and simplification.
-- These cleanups include:
-- 1. Removing negative exponents by converting them to division.
-- 2. Simplifying powers of one.
-- 3. Flattening nested powers.
-- 4. Converting multiplications by -1 into negations.

-- Main cleanup function
cleanupExpr :: Expr -> Expr
cleanupExpr = fixpoint cleanupOnce

-- Single pass cleanup
cleanupOnce :: Expr -> Expr
cleanupOnce =
    cleanupNegPowers
  . cleanupPowPow
  . cleanupPowOne
  . cleanupNegOneOnVar
  -- . pushNegAdd
  . mapChildren cleanupOnce -- Recursively apply cleanup to children

-- This function maps a function over all children of an expression, over recognized nodes.
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

-- Cleanup negative powers by converting them back to division. We use negative powers since its easier to handle during normalization. 
cleanupNegPowers :: Expr -> Expr
cleanupNegPowers = \case
  Mul a (Pow x (Num n)) | numberLtZero n ->
    Div a (Pow x (Num (negNum n)))

  Mul (Pow x (Num n)) a | numberLtZero n ->
    Div a (Pow x (Num (negNum n)))

  e -> e

-- Remove powers of one: x^1 -> x
cleanupPowOne :: Expr -> Expr
cleanupPowOne = \case
  Pow a (Num n) | isOne n -> a
  e -> e

-- Flatten nested powers: (x^a)^b -> x^(a*b)
cleanupPowPow :: Expr -> Expr
cleanupPowPow = \case
  Pow (Pow a b) c -> Pow a (Mul b c)
  e -> e

-- Convert multiplications by -1 into negations: -1 * x -> -x
cleanupNegOneOnVar :: Expr -> Expr
cleanupNegOneOnVar = \case
  Mul (Num n) e | isNegOne n -> Neg e
  Mul e (Num n) | isNegOne n -> Neg e
  e -> e

-- Helper to detect exactly -1
isNegOne :: Number -> Bool
isNegOne (R r) = r == -1
isNegOne (D d) = d == -1

pushNegAdd :: Expr -> Expr
pushNegAdd = \case
  Add a (Neg b) -> Sub a b
  Add (Neg a) b -> Sub b a
  e -> e