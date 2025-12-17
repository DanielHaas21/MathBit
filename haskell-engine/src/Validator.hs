{-# LANGUAGE DeriveGeneric #-}

module Validator ( 
    validator, StepData(..)
)where

import Data.Aeson
import Struct.Expr (Expr(..))

-- This module exports a validation functions that takes an Expr and identifies important math data 
-- result of the validation function is an object containing flags, This is very important for optimization since it tells us which rule files to traverse through and which not for a given step 

-- data type
data StepData = StepData
  { hasCalculus       :: Bool
  , hasCombinatorics  :: Bool
  , hasTrigonometry  :: Bool
  , hasFunction      :: Bool
  } deriving (Show, Eq)

-- instance - ible step used for merging
emptyStep :: StepData
emptyStep = StepData
  { hasCalculus      = False
  , hasCombinatorics = False
  , hasTrigonometry = False
  , hasFunction     = False
  }

-- merges stepdata
merge :: StepData -> StepData -> StepData
merge a b = StepData
  { hasCalculus      = hasCalculus a      || hasCalculus b
  , hasCombinatorics = hasCombinatorics a || hasCombinatorics b
  , hasTrigonometry = hasTrigonometry a || hasTrigonometry b
  , hasFunction     = hasFunction a     || hasFunction b
  }

-- Function that validates the entire Expr, Each step has the function itself called and merged recursively, number of calls per symbol is determined via how many arguments each symbol has   
validator :: Expr -> StepData
validator expr = case expr of

    -- Atomical data, nothing special
    Var _ -> emptyStep
    Num _ -> emptyStep
    ConstantPi -> emptyStep
    ConstantE  -> emptyStep

    -- Arithmetic (always allowed, no flags), is binary therefore needs merging
    Add a b -> validator a `merge` validator b
    Sub a b -> validator a `merge` validator b
    Mul a b -> validator a `merge` validator b
    Div a b -> validator a `merge` validator b
    Pow a b -> validator a `merge` validator b
    Neg a   -> validator a

    -- unary functions
    Func name a ->
        (validator a) `merge`
        emptyStep { hasFunction = True
                    , hasTrigonometry = isTrig name -- we have to also check trig. functions since they have their own ruleset 
                    }

    -- similar to aritmethic
    Abs a        -> validator a
    Sqrt a       -> validator a
    Root n a     -> validator n `merge` validator a

    -- Calculus
    Integral a ->
        (validator a) `merge` emptyStep { hasCalculus = True }
    -- has 3 arguments
    DefIntegral l u a ->
        validator l `merge` validator u `merge`
        (validator a `merge` emptyStep { hasCalculus = True })

    Limit _ to a ->
        validator to `merge`
        (validator a `merge` emptyStep { hasCalculus = True })

    -- Combinatorics
    Combination a b ->
        validator a `merge` validator b `merge`
        emptyStep { hasCombinatorics = True }

    Permutation a b ->
        validator a `merge` validator b `merge`
        emptyStep { hasCombinatorics = True }

    Variation a b ->
        validator a `merge` validator b `merge`
        emptyStep { hasCombinatorics = True }
    Factorial a ->
        validator a `merge` emptyStep { hasCombinatorics = True }

        
    -- Prime notation (calculus-adjacent)
    Prime a _ ->
        (validator a) `merge` emptyStep { hasCalculus = True }

-- Helper to check for trig. functions
isTrig :: String -> Bool
isTrig name = name `elem`
  [ "sin", "cos", "tan"
  , "cot", "sec", "csc"
  , "arcsin", "arccos", "arctan"
  ]