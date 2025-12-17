{-# LANGUAGE LambdaCase #-}

module Rules.Algebra
  ( rules
  ) where

import Struct.Expr
import Struct.Rule
import Data.List (groupBy, partition, sortOn)
import Data.Function (on)
import qualified Data.Map.Strict as M

-- | All algebra rules (n-ary aware)
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
  ]

-- x^a / x^b -> x^(a-b)
-- Does similar grouping as mulSameBaseN but for division, but is way simpler since its binary
divSameBaseN :: Rule
divSameBaseN = Rule "divSameBaseN" "skib" 20 $ \case
  Div (Pow x a) (Pow y b) | x == y -> Just (Pow x (Sub a b))
  Div x@(Var _) y@(Var _) | x == y -> Just (Num 1)
  _ -> Nothing

-- (x^a)^b -> x^(a*b)
powPow :: Rule
powPow = Rule "powPow" "skib"  20 $ \case
  Pow (Pow x a) b -> Just (Pow x (Mul a b))
  _ -> Nothing

-- x^0 -> 1
powZero :: Rule
powZero = Rule "powZero" "skib" 10 $ \case
  Pow (Num 0) _ -> Just (Num 0)
  Pow _ (Num 0) -> Just (Num 1)
  _             -> Nothing

-- Sqrt(x) -> x^(1/2)
sqrtToPow :: Rule
sqrtToPow = Rule "sqrtToPow" "skib" 10 $ \case
  Sqrt x -> Just (Pow x (Num 0.5))
  _      -> Nothing

-- Root n x -> x^(1/n)
rootToPow :: Rule
rootToPow = Rule "rootToPow" "skib" 10 $ \case
  Root x n -> Just (Pow x (Div (Num 1) n))
  _        -> Nothing

-- ====================================
-- NEGATION
-- ====================================

-- --x -> x
negNeg :: Rule
negNeg = Rule "negNeg" "skib" 10 $ \case
  Neg (Neg x) -> Just x
  _           -> Nothing


-- a*(b + c + ...) -> a*b + a*c + ...
-- Distributes multiplication over addition for n-ary Add
distributeMulN :: Rule
distributeMulN = Rule "distributeMulN" "skib" 5 $ \case
  Mul a b ->
    let terms = collectMul (Mul a b) -- collect all mul terms
        maybeAdd = filter isAdd terms -- find any Add terms
    in case maybeAdd of
         [] -> Nothing  -- no addition found, do nothing
         (Add x y : _) -> -- found an addition, distribute over it
           let before = takeWhile (/= Add x y) terms -- before
               after  = drop (length before + 1) terms -- after
               expanded = map (\t -> foldl1 Mul (before ++ [t] ++ after)) [x, y] -- map each term in the addition to a new multiplication
           in Just (foldl1 Add expanded) -- fold back into an addition 
  _ -> Nothing
  where -- helper to identify Add terms
    isAdd (Add _ _) = True
    isAdd _ = False

-- ====================================
-- HELPERS
-- ====================================


partitionNums :: [Expr] -> ([Double], [Expr])
partitionNums [] = ([], [])
partitionNums (Num x : xs) =
  let (ns, es) = partitionNums xs in (x:ns, es)
partitionNums (e:xs) =
  let (ns, es) = partitionNums xs in (ns, e:es)

collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e         = [e]

collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e         = [e]
