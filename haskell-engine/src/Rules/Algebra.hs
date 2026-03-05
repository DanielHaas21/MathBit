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
import Helpers.Numbers (numberLtZero, isOne, negNum)
import Data.Ratio (numerator, denominator)

--  All algebra rules
rules :: [Rule]
rules =
  [ 
  distributeMulN
  , powMulDistribN
  , addFractionsN
  ]


-- a*(b + c + ...) -> a*b + a*c + ...
-- Distributes multiplication over addition for n-ary Add
distributeMulN :: Rule
distributeMulN = Rule "distributeMulN" "When multiplying a term by a sum, distribute the multiplication over each addend" 5 Rewriting $ \case
  Mul a b -- we dont distribute if we have a division or negative power in the expression, as that would lead to feedback loops with the rules that turn divisions into multiplications and negative powers into positive ones
    | containsDiv (Mul a b) -> Nothing 
    | containsNegPow (Mul a b) -> Nothing
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

containsNegPow :: Expr -> Bool
containsNegPow = any isNegPow . collectMul

-- a/b + c/d -> (a*d + c*b) / (b*d)
-- Works on the internal representation after divToMul: fractions are Mul n (Pow d (Num -1))
-- Same denominator case: a/d + b/d -> (a+b)/d
addFractionsN :: Rule
addFractionsN = Rule "addFractionsN"
  "Add fractions by finding a common denominator: a/b + c/d = (a·d + c·b)/(b·d)"
  10 Rewriting $ \case
    Add a b ->
      case (extractFrac a, extractFrac b) of
        (Just (nA, dA), Just (nB, dB)) ->
          if dA == dB
            then Just (Mul (Add nA nB) (Pow dA (Num (R (-1))))) -- same denominator case
            else Just (Mul (Add (Mul nA dB) (Mul nB dA)) (Pow (Mul dA dB) (Num (R (-1))))) -- general case
        _ -> Nothing
    _ -> Nothing

-- Decompose an expression into (numerator, denominator) if it contains negative-exponent factors.
-- Works on the internal form produced by divToMul:
--   5 * (x-4)^(-1)     ->  Just (5, x-4)
--   (x+2)^(-1)         ->  Just (1, x+2)
--   5 * (x-4)^(-1) * (x+2)^(-1)  ->  Just (5, (x-4)*(x+2))
extractFrac :: Expr -> Maybe (Expr, Expr)
extractFrac e =
  let factors = collectMul e
      (denPows, numFactors) = partition isNegPow factors
  in if null denPows
       then Nothing
       else
         let numExpr   = fracProduct numFactors
             denomExpr = fracProduct (map invertNegPow denPows)
         in Just (numExpr, denomExpr)

-- True when an expression is a factor with a strictly negative exponent: x^(-n)
isNegPow :: Expr -> Bool
isNegPow (Pow _ (Num n))         = numberLtZero n
isNegPow (Pow _ (Mul (Num n) _)) = numberLtZero n  -- Mul (Num -1) e before folding
isNegPow _                       = False

-- Flip the sign of a negative-exponent factor back to its positive denominator.
-- Pow b (Num -1) -> b;  Pow b (Num -3) -> Pow b (Num 3)
invertNegPow :: Expr -> Expr
invertNegPow (Pow b (Num n)) =
  let pos = negNum n
  in if isOne pos then b else Pow b (Num pos)
invertNegPow (Pow b e) = Pow b (Mul (Num (R (-1))) e)
invertNegPow e         = e

-- Safe product: empty list -> 1, singleton -> itself, otherwise fold
fracProduct :: [Expr] -> Expr
fracProduct []  = Num (R 1)
fracProduct [x] = x
fracProduct xs  = foldl1 Mul xs

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


