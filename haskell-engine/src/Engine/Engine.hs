{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine.Engine( 
  simplify,
  simplifyWithLog,
) where

import Struct.Rule
import Struct.Expr
import Struct.Step
import Validator (StepData(..), validator)
import qualified Rules.Algebra as Algebra
import qualified Rules.Trigonometry as Trig
import qualified Rules.Calculus as Calc
import qualified Rules.Combinatorics as Comb
import Data.List (sortOn,groupBy)
import qualified Data.Map.Strict as M
import Engine.Normalize (normalize)
import Engine.Fold (foldNary, foldConstants)
import Helpers.Collect (collectAdd, collectMul)

-- This is the main Engine that imports Rules and contains all important logic

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
-- we often use go which is a generic tail recursive function, we can create abstracted arguments and the recursion is the last thing that happens 
rewrite :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewrite rules expr = 
    -- Recursive helper function with step counter to avoid infinite loops
    let (finalExpr, log) = go expr [] 0
    in (finalExpr, filterRepeatedSteps log)
  where
    maxSteps = 1000 -- max step cap to avoid loops
    go e log n -- e is current expr, log is accumulated logs (steps), n is step count
      | n > maxSteps = (e, log) -- return if max steps exceeded
      | otherwise = 
          let (e', log1) = rewriteOnce rules e -- rewrite 
          in if e' == e -- if no change(first expr e and next step epxr e' match) and merge up logs, return 
             then (e, log ++ log1)
             else go e' (log ++ log1) (n+1) -- if they dont we increment the step index

-- Each Rewrite 
rewriteOnce :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewriteOnce rules expr =
  case rewriteChildren rules expr of -- We recurse through the data structure and rewrite each child
    (expr', log1) -> -- Each expr here is essentially a single element in the expr
      let
        folded     = foldConstants expr' -- for each element we try fold/ evaulate its constants
        normalized = normalize folded   -- and normalize its symbolic properties
      in
        case applyRules rules normalized of -- we then try to apply rules to the normalized part
          Just (newExpr, step)
            | newExpr /= normalized -> (newExpr, log1 ++ [step]) -- if the normalized is different from the new rule-applied expr we add a next step to the parent
            | otherwise             -> (normalized, log1) -- if its the same we dont add a step
          Nothing -> (normalized, log1) -- failsafe

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

-- Rewrite a list of expressions and collect logs
rewriteNary :: [Rule] -> [Expr] -> ([Expr], SimplifyLog)
rewriteNary rules exprs =
  let (exprs', logs) = unzip $ map (rewrite rules) exprs
  in (exprs', concat logs)

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

      