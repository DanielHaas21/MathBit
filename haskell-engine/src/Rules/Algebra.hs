{-# LANGUAGE LambdaCase #-}

module Rules.Algebra
  ( rules
  ) where

import Struct.Expr
import Struct.Rule
-- Type for a single simplification rule
-- List of algebra rules

rules :: [Rule]
rules =
  [ addZero
--   , mulOne
--   , mulZero
--   , subZero
--   , divOne
--   , negNeg
  ]

-- Rule implementations

-- x + 0 -> x
addZero :: Rule
addZero = Rule "addZero" $ \case
    Add x (Num 0) -> Just x
    Add (Num 0) x -> Just x
    _             -> Nothing

-- x * 1 -> x
-- mulOne :: Rule
-- mulOne (Mul x (Num 1)) = Just x
-- mulOne (Mul (Num 1) x) = Just x
-- mulOne _ = Nothing

-- -- x * 0 -> 0
-- mulZero :: Rule
-- mulZero (Mul _ (Num 0)) = Just (Num 0)
-- mulZero (Mul (Num 0) _) = Just (Num 0)
-- mulZero _ = Nothing

-- -- x - 0 -> x
-- subZero :: Rule
-- subZero (Sub x (Num 0)) = Just x
-- subZero _ = Nothing

-- -- x / 1 -> x
-- divOne :: Rule
-- divOne (Div x (Num 1)) = Just x
-- divOne _ = Nothing

-- -- --(-x) -> x
-- negNeg :: Rule
-- negNeg (Neg (Neg x)) = Just x
-- negNeg _ = Nothing
