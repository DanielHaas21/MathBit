{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
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
import Data.Ratio (numerator, denominator)

--  All algebra rules
rules :: [Rule]
rules =
  [ 
  distributeMulN
  , powMulDistribN
  ]


-- a*(b + c + ...) -> a*b + a*c + ...
-- Distributes multiplication over addition for n-ary Add
distributeMulN :: Rule
distributeMulN = Rule "distributeMulN" "When multiplying a term by a sum, distribute the multiplication over each addend" 5 Rewriting $ \case
  Mul a b
    | containsDiv (Mul a b) -> Nothing
    | otherwise ->
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

containsDiv :: Expr -> Bool
containsDiv = \case
  Div _ _ -> True
  Add a b -> containsDiv a || containsDiv b
  Mul a b -> containsDiv a || containsDiv b
  Pow a b -> containsDiv a || containsDiv b
  _       -> False

-- (a*b*c)^n -> a^n * b^n * c^n
-- Same as DistributeMulN but for powers, takes a collection of mul terms and raises each to the power n 
powMulDistribN :: Rule
powMulDistribN = Rule
  "powMulDistribN"
  "When raising a product to a power, distribute the power to each factor: (a*b*c)^n -> a^n * b^n * c^n..."
  5 Rewriting $ \case
    Pow e n ->
      case collectMul e of
        [_] -> Nothing  -- not actually a product
        xs  -> Just (foldNary Mul (map (\x -> Pow x n) xs))
    _ -> Nothing


