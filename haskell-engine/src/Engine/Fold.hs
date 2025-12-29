{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Fold (foldConstants, foldNary) where

import Struct.Expr
import Data.List (sortOn,groupBy)
import Data.Ratio (numerator, denominator)
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
    let xs = collectAdd (Add a b)
        xs' = map foldConstants xs
        (nums, others) = partitionNums xs'
        numSum = foldl addNum (R 0) nums
        allTerms = if not (isZero numSum) then Num numSum : others else others
    in case allTerms of
         []  -> Num (R 0)
         [x] -> x
         xsN -> foldl1 Add xsN

  -- N-ary Mul
  Mul a b ->
    let xs = collectMul (Mul a b)
        xs' = map foldConstants xs
        (nums, others) = partitionNums xs'
        numProd = foldl mulNum (R 1) nums
        finalTerms = if not (isOne numProd) then Num numProd : others else others
    in case finalTerms of
         []  -> Num (R 1)
         [x] -> x
         xsN -> foldl1 Mul xsN

  -- Negation
  Neg e ->
    let e' = foldConstants e
    in case e' of
         Num n -> Num (negNum n)
         Neg x -> x
         _     -> Neg e'

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

-- Number helpers

addNum :: Number -> Number -> Number
addNum (R x) (R y) = R (x + y)
addNum (D x) (D y) = D (x + y)
addNum (R x) (D y) = D (fromRational x + y)
addNum (D x) (R y) = D (x + fromRational y)

mulNum, divNum, powNum :: Number -> Number -> Number

mulNum (R x) (R y) = R (x * y)
mulNum (D x) (D y) = D (x * y)
mulNum (R x) (D y) = D (fromRational x * y)
mulNum (D x) (R y) = D (x * fromRational y)

divNum (R x) (R y) = R (x / y)
divNum (D x) (D y) = D (x / y)
divNum (R x) (D y) = D (fromRational x / y)
divNum (D x) (R y) = D (x / fromRational y)

powNum (R x) (R y)
  | denominator y == 1 = R (x ^^ numerator y) -- integer powers exact
  | otherwise          = D (fromRational x ** fromRational y)
powNum (D x) (D y) = D (x ** y)
powNum (R x) (D y) = D (fromRational x ** y)
powNum (D x) (R y) = D (x ** fromRational y)

isZero, isOne :: Number -> Bool
isZero (R x) = x == 0
isZero (D x) = x == 0
isOne  (R x) = x == 1
isOne  (D x) = x == 1

negNum :: Number -> Number
negNum (R x) = R (-x)
negNum (D x) = D (-x)

absNum :: Number -> Number
absNum (R x) = R (abs x)
absNum (D x) = D (abs x)

isIntegerNum :: Number -> Bool
isIntegerNum (R x) = denominator x == 1
isIntegerNum (D x) = x == fromInteger (round x)

integerPart :: Number -> Integer
integerPart (R x) = numerator x
integerPart (D x) = round x


-- Helper to partition numeric and non-numeric expressions
partitionNums :: [Expr] -> ([Number], [Expr])
partitionNums [] = ([], [])
partitionNums (Num n : xs) =
  let (ns, es) = partitionNums xs in (n:ns, es)
partitionNums (e:xs) =
  let (ns, es) = partitionNums xs in (ns, e:es)

-- Collect all nested Add / Mul for n-ary flattening
collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e         = [e]

collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e         = [e]
