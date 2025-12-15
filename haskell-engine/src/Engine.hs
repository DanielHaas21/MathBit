{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Engine
    ( simplify
) where

import Struct.Rule
import Struct.Expr
import Validator (StepData(..), validator)
import qualified Rules.Algebra as Algebra
import qualified Rules.Trigonometry as Trig
import qualified Rules.Calculus as Calc
import qualified Rules.Combinatorics as Comb

data Step = Step
  { before :: Expr
  , after  :: Expr
  , rule   :: String
  } deriving (Show)

type SimplifyLog = [Step]

simplify :: Expr -> Expr
simplify = fst . simplifyWithLog

simplifyWithLog :: Expr -> (Expr, SimplifyLog)
simplifyWithLog expr =
    let meta  = validator expr
        rules = selectRules meta
    in rewrite rules expr


selectRules :: StepData -> [Rule]
selectRules meta =
  Algebra.rules
    ++ if hasTrigonometry meta then Trig.rules else []
    ++ if hasCalculus meta      then Calc.rules else []
    ++ if hasCombinatorics meta then Comb.rules else []

rewrite :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewrite rules expr =
  let (expr', log1) = rewriteOnce rules expr
  in if expr' == expr
        then (expr, log1)
        else
          let (final, log2) = rewrite rules expr'
          in (final, log1 ++ log2)

rewriteOnce :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewriteOnce rules expr =
  case rewriteChildren rules expr of
    (expr', log1) ->
      case applyRules rules expr' of
        Just (newExpr, step) ->
          (newExpr, log1 ++ [step])
        Nothing ->
          (expr', log1)

rewriteChildren :: [Rule] -> Expr -> (Expr, SimplifyLog)
rewriteChildren rules = \case

  Add a b ->
    let (a', la) = rewrite rules a
        (b', lb) = rewrite rules b
    in (Add a' b', la ++ lb)

  Sub a b ->
    let (a', la) = rewrite rules a
        (b', lb) = rewrite rules b
    in (Sub a' b', la ++ lb)

  Mul a b ->
    let (a', la) = rewrite rules a
        (b', lb) = rewrite rules b
    in (Mul a' b', la ++ lb)

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

  Func f a ->
    let (a', l) = rewrite rules a
    in (Func f a', l)

  -- atomic
  e -> (e, [])

applyRules :: [Rule] -> Expr -> Maybe (Expr, Step)
applyRules rules expr =
  foldr tryRule Nothing rules
  where
    tryRule r acc =
      case acc of
        Just _ -> acc
        Nothing ->
          case apply r expr of
            Just e' ->
              Just (e', Step expr e' (ruleName r))
            Nothing ->
              Nothing