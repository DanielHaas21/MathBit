{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Normalize (normalize) where

import Struct.Expr
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Helpers.Partition (partitionNums)
import Data.Ratio (numerator, denominator)
import Helpers.Collect (collectAdd, collectMul)
import Helpers.FixPoint (fixpoint)
import Helpers.Numbers (addNum, mulNum, isOne, isZero)

-- The normalization module that puts expressions into a canonical form and 
-- combines like terms.
-- This is done via collecting terms in addition and multiplication, summing  
-- coefficients for like terms, and rebuilding the expression.
-- It operates recursively on the expression tree. 
-- And also uses both R and D number types for numerical accuracy.

-- R D helpers are used here aswell

-- Main normalization function
normalize :: Expr -> Expr
normalize = fixpoint normalizeExpr

-- Normalize an expression recursively until a fixed point is reached
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

-- Multiplication
normalizeMul :: [Expr] -> Expr
normalizeMul terms =
  let
    ts = map normalizeExpr terms -- normalize each term first and map it
    (nums, rest) = partitionNums ts -- once again partition into numbers and others
    coeff = foldl mulNum (R 1) nums  -- use mulNum to multiply all numbers together to get overall coefficient
    powerMap = foldr insertPower M.empty rest -- insert each term into a map of base -> exponent

    -- used to add two exponents together, used when we have the same base and need to combine them
    syms = [ if isOneExpr expExpr then base else Pow base expExpr -- rebuild each base^exponent from the map, if its exponent 1 just use base
           | (base, expRaw) <- M.toList powerMap -- we create a list of (base, exponent) pairs from the map
           , let expExpr = normalizeExpr expRaw -- we normalize the exponent expression
           , not (isZeroExpr expExpr) -- skip zero exponents
           ]
    final = (if not (isOne coeff) then [Num coeff] else []) ++ syms -- combine coefficient with symbolic parts and build final list
  in
    case final of
      []  -> Num (R 1) -- if nothing left, return 1
      [x] -> x -- if only one term left, return it
      xs  -> foldl1 Mul xs -- else fold it back to n-ary Mul

-- insert a term into the power map (base -> exponent)
insertPower :: Expr -> M.Map Expr Expr -> M.Map Expr Expr
insertPower e m = case e of
  Pow b n -> M.insertWith addExp b n m
  _       -> M.insertWith addExp e (Num (R 1)) m

addExp :: Expr -> Expr -> Expr
addExp new old = normalizeExpr (Add old new)

-- special case for multiplying by a power with a negative exponent: x * x^(-n) -> x^(1-n)
-- this is needed to handle cases where we have something like x * (1/x) 
-- which would be represented as x * x^(-1) after normalization, and we want to combine them into x^(1-1) = x^0 = 1
isZeroExpr :: Expr -> Bool
isZeroExpr (Num n) = isZero n
isZeroExpr _ = False

isOneExpr :: Expr -> Bool
isOneExpr (Num n) = isOne n
isOneExpr _ = False


-- Addition
normalizeAdd :: [Expr] -> Expr
normalizeAdd terms =
  let
    ts = map normalizeExpr terms -- normalize each term first and map it
    grouped = foldr insertTerm M.empty ts -- group terms by their symbolic part, summing coefficients
    rebuilt = [ rebuild c k | (k,c) <- M.toList grouped, not (isZero c) ] -- rebuild terms with non-zero coefficients
  in case rebuilt of
       []  -> Num (R 0) -- if nothing left, return 0
       [x] -> x -- if only one term left, return it
       xs  -> foldl1 Add xs -- else fold it back to n-ary Add

-- insert a term into the addition map (symbolic part -> coefficient) 
insertTerm :: Expr -> M.Map Expr Number -> M.Map Expr Number
insertTerm e m =
  let (c, k) = splitCoeff e
  in M.insertWith addNum k c m

-- split an expression into its coefficient and symbolic part
splitCoeff :: Expr -> (Number, Expr)
splitCoeff e =
  let factors = collectMul e -- collect factors of the expression
      (nums, syms) = partitionNums factors -- partition into numbers and symbolic parts
      coeff = foldl mulNum (R 1) nums -- multiply numbers to get overall coefficient and foldl 
      key = case syms of 
              []  -> Num (R 1) -- if no symbolic parts, key is 1
              [x] -> x -- if only one symbolic part, key is that part
              xs  -> foldl1 Mul xs -- else fold back to n-ary Mul
  in (coeff, key)

-- rebuild an expression from its coefficient and symbolic part back into a full expression
rebuild :: Number -> Expr -> Expr
rebuild c k
  | k == Num (R 1) = Num c
  | isOne c         = k
  | otherwise       = Mul (Num c) k

