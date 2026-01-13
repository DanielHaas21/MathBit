{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Rules.Structural
  ( rules
  ) where

import Struct.Expr
import Struct.Rule
import Data.List (groupBy, partition, sortOn)
import Helpers.Partition (partitionNums)
import Data.Function (on)
import Engine.Fold (foldNary)
import qualified Data.Map.Strict as M
import Helpers.Collect (collectAdd,collectMul)
import Data.Ratio (numerator, denominator)


-- All structural rules
-- Structural rules are used for normalization and enabling cancellations. They are applied before any algebraic rules.
-- They allow us to create a standard form for expressions so that we can catch any simplification opportunities that would otherwise be missed.
rules :: [Rule]
rules =
  [ 
  negNeg 
  , divToMul
  , powZero
  , divSameBaseN
  , powPow
  , sqrtToPow
  , rootToPow
  ]

-- Cancel div if same base: x^a / x^b -> x^(a-b), x / x -> 1
divSameBaseN :: Rule
divSameBaseN = Rule "divSameBaseN" "Denominator is the same base, therefore cancel division" 20 Structural $ \case
  Div (Pow x a) (Pow y b) | x == y -> Just (Pow x (Sub a b))
  Div x@(Var _) y@(Var _) | x == y -> Just (Num (R 1))
  _ -> Nothing

-- Power to power: (x^a)^b -> x^(a*b)
powPow :: Rule
powPow = Rule "powPow" "Add up exponents inside powers"  20 Structural $ \case
  Pow (Pow x a) b -> Just (Pow x (Mul a b))
  _ -> Nothing


-- x^0 -> 1
powZero :: Rule
powZero = Rule "powZero" "Any base raised to the power of 0 is 1" 10 Structural $ \case
  Pow (Num (R 0)) _ -> Just (Num (R 0))
  Pow _ (Num (R 0)) -> Just (Num (R 1))
  _             -> Nothing

-- Sqrt(x) -> x^(1/2)
sqrtToPow :: Rule
sqrtToPow = Rule "sqrtToPow" "Square root is converted to power of 1/2" 10 Structural $ \case
  Sqrt x -> Just (Pow x (Num  (R 0.5)))
  _      -> Nothing

-- Root n x -> x^(1/n)
rootToPow :: Rule
rootToPow = Rule "rootToPow" "N-th root is converted to power of 1/n" 10 Structural $ \case
  Root x n -> Just (Pow x (Div (Num (R 1)) n))
  _        -> Nothing

-- --x -> x
negNeg :: Rule
negNeg = Rule "negNeg" "negation of negation is positive" 10 Structural $ \case
  Neg (Neg x) -> Just x
  _           -> Nothing

-- a / b  -> a * b^(-1)
divToMul :: Rule
divToMul = Rule "divToMul" "Division is converted to multiplication by the reciprocal" 120 Structural $ \case
  Div a b -> Just (Mul a (Pow b (Num (R (-1)))))
  _       -> Nothing