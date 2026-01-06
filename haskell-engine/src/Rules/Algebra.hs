{-# LANGUAGE LambdaCase #-}

module Rules.Algebra
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

--  All algebra rules (n-ary aware)
rules :: [Rule]
rules =
  [ 
  negNeg
  , powZero
  , divSameBaseN
  , powPow
  , sqrtToPow
  , rootToPow
  , distributeMulN
  , divToMul
  , powMulDistribN
  ]

-- x^a / x^b -> x^(a-b)
-- Does similar grouping as mulSameBaseN but for division, but is way simpler since its binary
divSameBaseN :: Rule
divSameBaseN = Rule "divSameBaseN" "skib" 20 $ \case
  Div (Pow x a) (Pow y b) | x == y -> Just (Pow x (Sub a b))
  Div x@(Var _) y@(Var _) | x == y -> Just (Num (R 1))
  _ -> Nothing

-- (x^a)^b -> x^(a*b)
powPow :: Rule
powPow = Rule "powPow" "skib"  20 $ \case
  Pow (Pow x a) b -> Just (Pow x (Mul a b))
  _ -> Nothing


-- x^0 -> 1
powZero :: Rule
powZero = Rule "powZero" "skib" 10 $ \case
  Pow (Num (R 0)) _ -> Just (Num (R 0))
  Pow _ (Num (R 0)) -> Just (Num (R 1))
  _             -> Nothing

-- Sqrt(x) -> x^(1/2)
sqrtToPow :: Rule
sqrtToPow = Rule "sqrtToPow" "skib" 10 $ \case
  Sqrt x -> Just (Pow x (Num  (R 0.5)))
  _      -> Nothing

-- Root n x -> x^(1/n)
rootToPow :: Rule
rootToPow = Rule "rootToPow" "skib" 10 $ \case
  Root x n -> Just (Pow x (Div (Num (R 1)) n))
  _        -> Nothing

-- ====================================
-- NEGATION
-- ====================================

-- --x -> x
negNeg :: Rule
negNeg = Rule "negNeg" "skib" 10 $ \case
  Neg (Neg x) -> Just x
  _           -> Nothing


divToMul :: Rule
divToMul = Rule "divToMul" "a / b -> a * b^(-1)" 100 $ \case
  Div a b -> Just (Mul a (Pow b (Num (R (-1)))))
  _       -> Nothing

-- a*(b + c + ...) -> a*b + a*c + ...
-- Distributes multiplication over addition for n-ary Add
distributeMulN :: Rule
distributeMulN = Rule "distributeMulN" "skib" 5 $ \case
  Mul a b ->
    let terms = collectMul (Mul a b)
        maybeAdd = filter isAdd terms
    in case maybeAdd of
         [] -> Nothing
         (Add x y : _) ->
           let before = takeWhile (/= Add x y) terms
               after  = drop (length before + 1) terms
               expanded = map (\t -> foldl1 Mul (before ++ [t] ++ after)) [x, y]
           in Just (foldl1 Add expanded)
  _ -> Nothing
  where
    isAdd (Add _ _) = True
    isAdd _ = False

-- (a*b*c)^n -> a^n * b^n * c^n
-- Same as DistributeMulN but for powers, takes a collection of mul terms and raises each to the power n 
powMulDistribN :: Rule
powMulDistribN = Rule
  "powMulDistribN"
  "(∏ a_i)^n -> ∏ a_i^n"
  90 $ \case
    Pow e n ->
      case collectMul e of
        [_] -> Nothing  -- not actually a product
        xs  -> Just (foldNary Mul (map (\x -> Pow x n) xs))
    _ -> Nothing


-- ====================================
-- HELPERS
-- ====================================



