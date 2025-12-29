{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Normalize (normalize) where

import Struct.Expr
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ratio (numerator, denominator)

normalize :: Expr -> Expr
normalize = normalizeExpr

normalizeExpr :: Expr -> Expr
normalizeExpr = \case
  Add a b -> normalizeAdd (collectAdd (Add a b))
  Mul a b -> normalizeMul (collectMul (Mul a b))

  Sub a b -> Sub (normalizeExpr a) (normalizeExpr b)
  Div a b -> Div (normalizeExpr a) (normalizeExpr b)
  Pow a b -> Pow (normalizeExpr a) (normalizeExpr b)
  Neg a   -> Neg (normalizeExpr a)

  Func f a -> Func f (normalizeExpr a)
  Sqrt a   -> Sqrt (normalizeExpr a)
  Root n a -> Root (normalizeExpr n) (normalizeExpr a)

  e -> e

-- ----------------
-- Multiplication
-- ----------------
normalizeMul :: [Expr] -> Expr
normalizeMul terms =
  let
    ts = map normalizeExpr terms
    (nums, rest) = partitionNums ts
    coeff = foldl mulNum (R 1) nums  -- use mulNum
    powerMap = foldr insertPower M.empty rest

    syms = [ if isOne exp then base else Pow base (Num exp)
           | (base, exp) <- M.toList powerMap
           , not (isZero exp)
           ]
    final = (if not (isOne coeff) then [Num coeff] else []) ++ syms
  in
    case final of
      []  -> Num (R 1)
      [x] -> x
      xs  -> foldl1 Mul xs

insertPower :: Expr -> M.Map Expr Number -> M.Map Expr Number
insertPower e m = case e of
  Pow b (Num n) -> M.insertWith addNum b n m
  _             -> M.insertWith addNum e (R 1) m

-- ----------------
-- Addition
-- ----------------
normalizeAdd :: [Expr] -> Expr
normalizeAdd terms =
  let
    ts = map normalizeExpr terms
    grouped = foldr insertTerm M.empty ts
    rebuilt = [ rebuild c k | (k,c) <- M.toList grouped, not (isZero c) ]
  in case rebuilt of
       []  -> Num (R 0)
       [x] -> x
       xs  -> foldl1 Add xs

insertTerm :: Expr -> M.Map Expr Number -> M.Map Expr Number
insertTerm e m =
  let (c, k) = splitCoeff e
  in M.insertWith addNum k c m

splitCoeff :: Expr -> (Number, Expr)
splitCoeff e =
  let factors = collectMul e
      (nums, syms) = partitionNums factors
      coeff = foldl mulNum (R 1) nums
      key = case syms of
              []  -> Num (R 1)
              [x] -> x
              xs  -> foldl1 Mul xs
  in (coeff, key)

rebuild :: Number -> Expr -> Expr
rebuild c k
  | k == Num (R 1) = Num c
  | isOne c         = k
  | otherwise       = Mul (Num c) k

-- ----------------
-- Helpers
-- ----------------
partitionNums :: [Expr] -> ([Number],[Expr])
partitionNums [] = ([],[])
partitionNums (Num n:xs) =
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

isZero, isOne :: Number -> Bool
isZero (R x) = x == 0
isZero (D x) = x == 0
isOne  (R x) = x == 1
isOne  (D x) = x == 1

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