{-# LANGUAGE LambdaCase #-}

module Rules.Algebra
  ( rules
  ) where

import Struct.Expr
import Struct.Rule
import Data.List (groupBy, partition, sortOn)
import Data.Function (on)

-- | All algebra rules (n-ary aware)
rules :: [Rule]
rules =
  [ 
    addNumsN
  , addZeroN
  , subZeroN
  , mulZeroN
  , mulNumRightToLeft
  , divOne
  , negNeg
  , negZero
  , powZero
  , addNumsN
  , mulNumsN
  , mulSameBaseN
  , divSameBaseN
  , powPow
  , sqrtToPow
  , rootToPow
  , distributeMulN
  ]

-- ====================================
-- N-ARY ADDITION RULES
-- ====================================

-- x + 0 -> x, follows similar workflow as foldConstants in Engine.hs
addZeroN :: Rule
addZeroN = Rule "addZeroN" "skib" 10 $ \case
  Add a b ->
    let terms = collectAdd (Add a b)
        terms' = filter (/= Num 0) terms --  remove zeros
    in case terms' of
         []  -> Just (Num 0)
         [x] -> Just x
         xs  -> Just (foldl1 Add xs)
  _ -> Nothing

-- x - 0 -> x
subZeroN :: Rule
subZeroN = Rule "subZeroN" "skib" 10 $ \case
  Sub x (Num 0) -> Just x
  _             -> Nothing

-- Constant folding: sum numbers in n-ary Add, follows similar workflow as foldConstants in Engine.hs
addNumsN :: Rule
addNumsN = Rule "addNumsN" "skib" 30 $ \case
  Add a b ->
    let terms = collectAdd (Add a b)
        (nums, others) = partitionNums terms
        sumNums = sum nums
        allTerms = if sumNums /= 0 then Num sumNums : others else others
    in case allTerms of
         []  -> Just (Num 0)
         [x] -> Just x
         xs  -> Just (foldl1 Add xs)
  _ -> Nothing

-- ====================================
-- N-ARY MULTIPLICATION RULES
-- ====================================
-- Creates a standard form for multiplication by moving numbers to the left
mulNumRightToLeft :: Rule
mulNumRightToLeft = Rule "mulNumRightToLeft" "prio" 100 $ \case
  Mul x (Num c) | not (isNum x) -> Just (Mul (Num c) x)
  _ -> Nothing

isNum :: Expr -> Bool
isNum (Num _) = True
isNum _       = False

-- x * 1 -> x, follows similar workflow as foldConstants in Engine.hs
mulOneN :: Rule
mulOneN = Rule "mulOneN" "skib" 10 $ \case
  Mul a b ->
    let terms = collectMul (Mul a b)
        terms' = filter (/= Num 1) terms
    in if any (== Num 0) terms
       then Just (Num 0)
       else case terms' of
              []  -> Just (Num 1)
              [x] -> Just x
              xs  -> Just (foldl1 Mul xs)
  _ -> Nothing

-- x * 0 -> 0
mulZeroN :: Rule
mulZeroN = Rule "mulZeroN" "skib" 20 $ \case
  Mul a b ->
    let terms = collectMul (Mul a b)
    in if any (== Num 0) terms
       then Just (Num 0)
       else Nothing
  _ -> Nothing

-- Constant folding: multiply numbers in n-ary Mul, follows similar workflow as foldConstants in Engine.hs
mulNumsN :: Rule
mulNumsN = Rule "mulNumsN" "skib" 30 $ \case
  Mul a b ->
    let terms = collectMul (Mul a b)
        (nums, others) = partitionNums terms
        prodNums = product nums
        finalTerms = if prodNums /= 1 then Num prodNums : others else others
    in case finalTerms of
         []  -> Just (Num 1)
         [x] -> Just x
         xs  -> Just (foldl1 Mul xs)
  _ -> Nothing

-- ====================================
-- POW AND BASE RULES
-- ====================================

-- x^a * x^b * ... -> x^(a+b)
-- Groups all powers with the same base and sums their exponents
mulSameBaseN :: Rule
mulSameBaseN = Rule "mulSameBaseN" "skib" 20 $ \case
  Mul a b ->
    let terms = collectMul (Mul a b) -- collect all terms
        (powers, others) = partition isPow terms -- partition into powers and other
        grouped = groupBy ((==) `on` baseOf) powers -- group by base x^n
        newPowers = map (\grp -> Pow (baseOf (head grp)) (foldl1 Add (map exponentOf grp))) grouped -- sum exponents of all powers and map back to Pow
    in if null newPowers then Nothing  -- if no powers found, do nothing
       else Just (foldl1 Mul (newPowers ++ others)) 
  _ -> Nothing
  where -- helper to identify powers, bases and exponents
    isPow (Pow _ _) = True
    isPow (Var _)   = True
    isPow _         = False

    baseOf (Pow x _) = x
    baseOf v@(Var _) = v
    baseOf _         = error "Unexpected term in mulSameBaseN"

    exponentOf (Pow _ e) = e
    exponentOf (Var _)   = Num 1
    exponentOf _         = error "Unexpected term in mulSameBaseN"

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

-- x^1 -> x
powOne :: Rule
powOne = Rule "powOne" "skib" 10 $ \case
  Pow x (Num 1) -> Just x
  _             -> Nothing

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

-- -0 -> 0
negZero :: Rule
negZero = Rule "negZero" "skib" 10 $ \case
  Neg (Num 0) -> Just (Num 0)
  _           -> Nothing

-- ====================================
-- DIVISION
-- ====================================

-- x / 1 -> x
divOne :: Rule
divOne = Rule "divOne" "skib" 10 $ \case
  Div x (Num 1) -> Just x
  _             -> Nothing

-- ====================================
-- DISTRIBUTION
-- ====================================

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
