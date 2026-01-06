{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Fold (foldConstants, foldNary) where

import Struct.Expr
import Data.List (sortOn,groupBy)
import Data.Ratio (numerator, denominator)
import Helpers.Partition (partitionNums)
import Helpers.Collect (collectAdd, collectMul)
import Helpers.Numbers (addNum, mulNum, isOne, isZero, powNum, divNum, absNum)
-- Fold back into a n-ary Add / Mul
foldNary :: (Expr -> Expr -> Expr) -> [Expr] -> Expr
foldNary _ [] = Num (R 0)      -- empty Add -> 0 (or 1 for Mul if you adjust)
foldNary _ [x] = x
foldNary f (x:xs) = foldl f x xs

-- This function numerically evaluates pure numbers. That is, a case like 5 + x + 4 -> 9 + x
-- Here folding means combining numeric constants 
foldConstants :: Expr -> Expr
foldConstants = \case
  -- N-ary Add
  Add a b ->
    let xs = collectAdd (Add a b) -- first we collect all expressions
        xs' = map foldConstants xs -- we recurse over it and map it
        (nums, others) = partitionNums xs' -- we then partition them into the numbers and other
        numSum = foldl addNum (R 0) nums -- Numbers then added up from the left via foldl
        allTerms = if not (isZero numSum) then Num numSum : others else others -- if the sum isnt zero we return it
    in case allTerms of
         []  -> Num (R 0)
         [x] -> x
         xsN -> foldl1 Add xsN

  -- N-ary Mul
  Mul a b ->
    let xs = collectMul (Mul a b) -- first we collect all expressions
        xs' = map foldConstants xs -- we recurse over it and map it
        (nums, others) = partitionNums xs' -- we then partition them into the numbers and other
        numProd = foldl mulNum (R 1) nums -- Numbers then multiplied from the left via foldl
        finalTerms = if not (isOne numProd) then Num numProd : others else others  -- if the product isnt zero we return it
    in case finalTerms of
         []  -> Num (R 1)
         [x] -> x
         xsN -> foldl1 Mul xsN

  -- Pow
  Pow a b ->
    let a' = foldConstants a
        b' = foldConstants b
    in case (a', b') of
         (Num x, Num y) -> Num (powNum x y)
         _              -> Pow a' b'

  -- Div
  Div a b ->
    let a' = foldConstants a
        b' = foldConstants b
    in case (a', b') of
         (Num x, Num y) -> Num (divNum x y)
         _              -> Div a' b'

  Factorial e ->
    let e' = foldConstants e
    in case e' of
         Num n | isIntegerNum n && n >= (R 0)  ->
           Num . R . product $ map toRational [1..integerPart n]
         _ -> Factorial e'

  Abs e ->
    let e' = foldConstants e
    in case e' of
         Num n -> Num (absNum n)
         _     -> Abs e'

  -- Recurse through other unary operators
  Func f e -> Func f (foldConstants e)
  Sqrt e   -> Sqrt (foldConstants e)
  Root n e -> Root (foldConstants n) (foldConstants e)
  Integral f -> Integral (foldConstants f)
  DefIntegral l u f -> DefIntegral (foldConstants l) (foldConstants u) (foldConstants f)
  Derivative v f -> Derivative v (foldConstants f)
  Partial v f -> Partial v (foldConstants f)
  Prime f n -> Prime (foldConstants f) n
  Limit v to body -> Limit v (foldConstants to) (foldConstants body)
  Combination n k -> Combination (foldConstants n) (foldConstants k)
  Permutation n k -> Permutation (foldConstants n) (foldConstants k)
  Variation n k -> Variation (foldConstants n) (foldConstants k)

  -- Atomic
  e@(Var _) -> e
  Num n     -> Num n
  ConstantPi -> ConstantPi
  ConstantE  -> ConstantE

isIntegerNum :: Number -> Bool
isIntegerNum (R x) = denominator x == 1
isIntegerNum (D x) = x == fromInteger (round x)

integerPart :: Number -> Integer
integerPart (R x) = numerator x
integerPart (D x) = round x
