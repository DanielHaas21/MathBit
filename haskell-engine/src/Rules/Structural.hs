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
import Helpers.Numbers (numberLtZero, isOne)
import Engine.Normalize (normalize)
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
  , cancelFracN
  , powPow
  , sqrtToPow
  , rootToPow
  ]

-- Cancel a factor in a product against a matching denominator factor.
-- Works on the internal Mul form produced by divToMul.
-- Handles two cases:
--   exact match:        x * (x)^(-1)     -> 1     (same factor cancels)
--   additive inverse:   (5-x) * (x-5)^(-1) -> -1  (opposite factors cancel to -1)
-- Also handles mixed products:
--   x * (5-x) * (x-5)^(-1)  ->  -x
cancelFracN :: Rule
cancelFracN = Rule "cancelFracN"
  "Cancel a common factor between numerator and denominator, or simplify additive-inverse factors to -1"
  25 Structural $ \e ->
    let factors    = collectMul e -- we collect the factors in the expression into a list
        (denPows, numFactors) = partition isNegOnePow factors -- we then partition them into the ones that are negative powers (denominator factors) and the rest (numerator factors)
    in if null denPows || null numFactors -- once again if null then nothing to cancel
         then Nothing 
         else tryCancel numFactors (zip [0..] denPows) -- we then try to cancel any numerator factor against the denominator factors, we zip the denominator factors with their indices so we can remove them if they cancel
  where
    isNegOnePow (Pow _ (Num n)) = n == R (-1) || n == D (-1)
    isNegOnePow _               = False

    -- Walk numerator factors; for the first one that cancels with a denom factor,
    -- remove both and rebuild (inserting -1 if they were additive inverses).
    tryCancel :: [Expr] -> [(Int, Expr)] -> Maybe Expr
    tryCancel nums indexedDens = go (zip [0..] nums)
      where
        go [] = Nothing
        go ((ni, n) : rest) =
          case findMatch n indexedDens of -- we look for a match for the numerator factor n in the denominator factors, if we find one we get its index and whether its an additive inverse, if we dont find one we keep looking through the rest of the numerator factors
            Nothing       -> go rest
            Just (di, inv) -> -- if we find a match, we then build a new expression with that numerator factor and the matching denominator factor removed, and if they were additive inverses we also insert a -1 into the factors to account for that
              let sign    = [Num (R (-1)) | inv]
                  newNums = deleteAt ni nums
                  newDens = deleteAt di (map snd indexedDens)
                  combined = sign ++ newNums ++ newDens
              in Just (if null combined then Num (R 1) else foldNary Mul combined)

    findMatch :: Expr -> [(Int, Expr)] -> Maybe (Int, Bool) -- looks for a match for a numerator factor in the denominator factors, returns the index of the matching denominator factor and whether its an additive inverse match
    findMatch _ [] = Nothing
    findMatch n ((di, Pow d _) : rest)
      | n == d                            = Just (di, False)
      | normalize (Add n d) == Num (R 0) = Just (di, True)
      | otherwise                         = findMatch n rest
    findMatch n (_ : rest) = findMatch n rest

    deleteAt :: Int -> [a] -> [a] -- helper function to delete an element at a specific index from a list
    deleteAt _ []     = []
    deleteAt 0 (_:xs) = xs
    deleteAt k (x:xs) = x : deleteAt (k-1) xs

-- Cancel div if same base: x^a / x^b -> x^(a-b), x / x -> 1
divSameBaseN :: Rule
divSameBaseN = Rule "divSameBaseN" "Denominator is the same base, therefore cancel division" 20 Structural $ \case
  Div (Pow x a) (Pow y b) | x == y -> Just (Pow x (Add a (Mul (Num (R (-1))) b)))
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
  Div a b ->
    let denomFactors = collectMul b
        reciprocalFactors = map reciprocal denomFactors
    in Just (foldNary Mul (a : reciprocalFactors))
  _       -> Nothing

-- reciprocal helper used by divToMul
reciprocal :: Expr -> Expr
reciprocal = \case
  Div n d -> Div d n
  Pow b e -> Pow b (Mul (Num (R (-1))) e)
  e       -> Pow e (Num (R (-1)))