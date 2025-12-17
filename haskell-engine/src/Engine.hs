{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine( 
  simplify,
  simplifyWithLog,
  Step(..)
) where

import Struct.Rule
import Struct.Expr
import Validator (StepData(..), validator)
import qualified Rules.Algebra as Algebra
import qualified Rules.Trigonometry as Trig
import qualified Rules.Calculus as Calc
import qualified Rules.Combinatorics as Comb
import Data.List (sortOn,groupBy)

-- This is the main Engine that imports Rules and contains all important logic

-- Each step
data Step = Step
  { before :: Expr
  , after  :: Expr
  , description :: String
  , rule   :: String
  } deriving (Show)

type SimplifyLog = [Step]

-- Two variants for the simplification function are exported 

-- Takes only the "fst" arg of log therefore ignoring steps
simplify :: Expr -> Expr
simplify = fst . simplifyWithLog

--Full simiplifier with step logging
simplifyWithLog :: Expr -> (Expr, SimplifyLog)
simplifyWithLog expr =
    let meta  = validator expr -- first we take metadata for the inital step via the validator
        rules = selectRules meta -- selected the initial rules based on that
    in rewrite rules expr -- initiate rewriting and rest of the workflow

-- Rule sulect function, returns an array of selected rule functions
selectRules :: StepData -> [Rule]
selectRules meta =
  sortOn (negate . priority) $  -- All rules have a priority prop 
    Algebra.rules -- Algebra rules are always used, rest is concated into the algebra rules
      ++ if hasTrigonometry meta then Trig.rules else []
      ++ if hasCalculus meta      then Calc.rules else []
      ++ if hasCombinatorics meta then Comb.rules else []

-- Helper for rewrite, takes two given steps next to each other [0,1,2..] and checks if they are the same, if yes it removes them and returns a reduced map 
filterRepeatedSteps :: SimplifyLog -> SimplifyLog
filterRepeatedSteps = map head . groupBy (\s1 s2 -> rule s1 == rule s2 && before s1 == before s2)

-- Main rewrite function, takes rule array and expression, returns final expression and log of steps
rewrite :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewrite rules expr = 
    -- Recursive helper function with step counter to avoid infinite loops
    let (finalExpr, log) = go expr [] 0
    in (finalExpr, filterRepeatedSteps log)
  where
    maxSteps = 1000 -- max step cap to avoid loops
    go e log n -- e is current expr, log is accumulated log, n is step count
      | n > maxSteps = (e, log) -- return if max steps exceeded
      | otherwise = 
          let (e', log1) = rewriteOnce rules e -- rewrite 
          in if e' == e -- if no change and merge up logs, return
             then (e, log ++ log1)
             else go e' (log ++ log1) (n+1)

-- Happens on each rewrite step, first rewrites children, then applies rules to the current expr
rewriteOnce :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewriteOnce rules expr =
  case rewriteChildren rules expr of
    (expr', log1) -> -- takes rewritten children expr and log  
      let exprFolded = foldConstants expr' -- fold constants
      in case applyRules rules exprFolded of  
          Just (newExpr, step) -- try to apply rules to the folded expr
            | newExpr /= exprFolded -> (newExpr, log1 ++ [step])  -- only log if changed
            | otherwise             -> (exprFolded, log1)        -- skip no-op
          Nothing -> (exprFolded, log1)

-- Rewrites children of the expression based on its constructor
rewriteChildren :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewriteChildren rules = \case
  -- N-ary Add
  Add a b ->
    let (xs, logs) = rewriteNary rules (collectAdd (Add a b))
    in (foldNary Add xs, logs)

  -- N-ary Mul
  Mul a b ->
    let (xs, logs) = rewriteNary rules (collectMul (Mul a b))
    in (foldNary Mul xs, logs)

  -- everything else remains the same
  -- may be removed since negating addition is preferred
  Sub a b ->
    let (a', la) = rewrite rules a
        (b', lb) = rewrite rules b
    in (Sub a' b', la ++ lb)

  Div a b ->
    let (a', la) = rewrite rules a
        (b', lb) = rewrite rules b
    in (Div a' b', la ++ lb)

  Pow a b ->
    let (a', la) = rewrite rules a
        (b', lb) = rewrite rules b
    in (Pow a' b', la ++ lb)

  Neg a ->
    let (a', l) = rewrite rules a
    in (Neg a', l)

  Factorial a ->
    let (a', l) = rewrite rules a
    in (Factorial a', l)

  Abs a ->
    let (a', l) = rewrite rules a
    in (Abs a', l)

  Func f a ->
    let (a', l) = rewrite rules a
    in (Func f a', l)

  Sqrt a ->
    let (a', l) = rewrite rules a
    in (Sqrt a', l)

  Root n a ->
    let (n', ln) = rewrite rules n
        (a', la) = rewrite rules a
    in (Root n' a', ln ++ la)

  -- rest of constructors remain unchanged
  e@(Var _) -> (e, [])
  e@Num{}   -> (e, [])
  ConstantPi -> (ConstantPi, [])
  ConstantE  -> (ConstantE, [])

-- Collect all nested Add / Mul for n-ary flattening
collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e         = [e]

collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e         = [e]

-- Rewrite a list of expressions and collect logs
rewriteNary :: [Rule] -> [Expr] -> ([Expr], SimplifyLog)
rewriteNary rules exprs =
  let (exprs', logs) = unzip $ map (rewrite rules) exprs
  in (exprs', concat logs)

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

-- Apply rules to an expression, returning the first successful application
applyRules :: [Rule] -> Expr -> Maybe (Expr, Step)
applyRules rules expr =
  foldr tryRule Nothing rules
  where
    tryRule r acc =
      case acc of
        Just _ -> acc -- already found a rule
        Nothing -> -- try this rule
          case apply r expr of 
            Just e' ->
              Just (e', Step expr e' (ruleDescription r) (ruleName r)) -- return new expr and step
            Nothing ->
              Nothing -- no rule found

      