{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Fold (foldConstants, foldNary) where

import Struct.Expr
import Data.List (sortOn,groupBy)

-- Fold back into a n-ary Add / Mul
foldNary :: (Expr -> Expr -> Expr) -> [Expr] -> Expr
foldNary _ [] = Num 0      -- empty Add -> 0 (or 1 for Mul if you adjust)
foldNary _ [x] = x
foldNary f (x:xs) = foldl f x xs

-- This function numerically evaluates pure numbers. That is, a case like 5 + x + 4 -> 9 + x
-- Here folding means combining numeric constants 
foldConstants :: Expr -> Expr
foldConstants = \case
  -- N-ary Add
  Add a b -> -- take all temrs 
    let xs = collectAdd (Add a b) -- collect all add terms 
        xs' = map foldConstants xs -- recurse fold constants on each term
        (nums, others) = partitionNums xs' -- partition into numbers and others
        numSum = sum nums -- sum
        allTerms = if numSum /= 0 then Num numSum : others else others -- reassemble
    in case allTerms of 
         []  -> Num 0 -- nothing
         [x] -> x -- single term - no folding needed
         xsN -> foldl1 Add xsN -- fold back if more than one term

  -- N-ary Mul
  Mul a b -> -- take all temrs 
    let xs = collectMul (Mul a b) -- collect all mul terms 
        xs' = map foldConstants xs -- recurse fold constants on each term
        (nums, others) = partitionNums xs' -- partition into numbers and others
        numProd = product nums -- product
    in if numProd == 0 then Num 0
       else
         let finalTerms = if numProd /= 1 then Num numProd : others else others -- reassemble
         in case finalTerms of
              []  -> Num 1 -- nothing
              [x] -> x -- single term - no folding needed
              xsN -> foldl1 Mul xsN -- fold back if more than one term

  -- Negation
  Neg e ->
    let e' = foldConstants e
    in case e' of
         Num n -> Num (-n)
         Neg x -> x  -- double negation
         _     -> Neg e'

  -- Pow and Div with numeric simplification
  Pow a b ->
    let a' = foldConstants a
        b' = foldConstants b
    in case (a', b') of
         (Num x, Num y) -> Num (x ** y)
         _              -> Pow a' b'

  Div a b ->
    let a' = foldConstants a
        b' = foldConstants b
    in case (a', b') of
         (Num x, Num y) -> Num (x / y)
         _              -> Div a' b'

  Factorial e ->
    let e' = foldConstants e
    in case e' of
         Num n | n >= 0, n == fromInteger (round n) ->
           Num . fromInteger . product $ [1..round n]
         _ -> Factorial e'

  Abs e ->
    let e' = foldConstants e
    in case e' of
         Num n -> Num (abs n)
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


-- Helper to partition numeric and non-numeric expressions
partitionNums :: [Expr] -> ([Double], [Expr])
partitionNums [] = ([], [])
partitionNums (Num x : xs) =
  let (ns, es) = partitionNums xs in (x:ns, es) -- numeric case
partitionNums (e:xs) =
  let (ns, es) = partitionNums xs in (ns, e:es) -- non-numeric case

-- Collect all nested Add / Mul for n-ary flattening
collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e         = [e]

collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e         = [e]