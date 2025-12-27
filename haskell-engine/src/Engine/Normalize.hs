{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Normalize (normalize) where
 
import Struct.Expr
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

normalize :: Expr -> Expr
normalize = normalizeExpr
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

normalizeMul :: [Expr] -> Expr
normalizeMul terms =
  let
    -- normalize children first
    ts = map normalizeExpr terms

    -- extract numeric coefficient
    (nums, rest) = partitionNums ts
    coeff = product nums

    -- collect powers: base -> exponent
    powerMap =
      foldr insertPower mempty rest

    -- rebuild symbolic part
    syms =
      [ if exp == 1
          then base
          else Pow base (Num exp)
      | (base, exp) <- M.toList powerMap
      , exp /= 0
      ]
    final =
      (if coeff /= 1 then [Num coeff] else []) ++ syms
  in
    case final of
      []  -> Num 1
      [x] -> x
      xs  -> foldl1 Mul xs

insertPower :: Expr -> M.Map Expr Double -> M.Map Expr Double
insertPower e m =
  case e of
    Pow b (Num n) -> M.insertWith (+) b n m
    _             -> M.insertWith (+) e 1 m

normalizeAdd :: [Expr] -> Expr
normalizeAdd terms =
  let
    ts = map normalizeExpr terms

    grouped =
      foldr insertTerm mempty ts

    rebuilt =
      [ rebuild c k
      | (k, c) <- M.toList grouped
      , c /= 0
      ]
  in
    case rebuilt of
      []  -> Num 0
      [x] -> x
      xs  -> foldl1 Add xs


insertTerm :: Expr -> M.Map Expr Double -> M.Map Expr Double
insertTerm e m =
  let (c, k) = splitCoeff e
  in M.insertWith (+) k c m

splitCoeff :: Expr -> (Double, Expr)
splitCoeff e =
  let
    factors = collectMul e
    (nums, syms) = partitionNums factors
    coeff = product nums
    key =
      case syms of
        []  -> Num 1
        [x] -> x
        xs  -> foldl1 Mul xs
  in
    (coeff, key)

rebuild :: Double -> Expr -> Expr
rebuild c k
  | k == Num 1  = Num c
  | c == 1      = k
  | otherwise   = Mul (Num c) k

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