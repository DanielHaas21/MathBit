{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( fromMathJSON )
where

import Struct.Expr (Expr(..))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T

-- Top level dispatch function
fromMathJSON :: Value -> Parser Expr
fromMathJSON v = case v of
    Array arr  -> parseArray arr
    String s   -> pure (Var $ T.unpack s) -- either edge or error cases 
    Number n   -> pure (Num $ realToFrac n)  -- Numbers
    _          -> fail "Invalid MathJSON expression"



-- Main array parser
parseArray :: Vector Value -> Parser Expr
parseArray arr
    | V.null arr = fail "Empty array"
    | otherwise =
        case V.head arr of
            String op -> parseOp (T.unpack op) (V.tail arr)  
            _         -> fail "Array does not start with string operator"

-- Parses a limit if it has a tuple inside it, tuple is mainly used as a structure block 
parseLimitTuple :: Vector Value -> Parser (String, Value)
parseLimitTuple v = case V.toList v of
    ["Tuple", String var, toVal] ->
        pure (T.unpack var, toVal)
    _ ->
        fail "Invalid limit tuple"

-- Function table for unary functions, that is any that are a form of fn(x) 
functionTable :: [(String, String)]
functionTable =
    [ ("Sin", "sin")
    , ("Cos", "cos")
    , ("Tan", "tan")
    , ("Cot", "cot")
    , ("Ln",  "ln")
    , ("Log", "log")
    , ("Exp", "exp")
    , ("Arcsin", "arcsin")
    , ("Arccos", "arccos")
    , ("Arctan", "arctan")
    ]


-- Main operator parser
parseOp :: String -> Vector Value -> Parser Expr
parseOp op args = case op of

    -- Basic arithmetic
    "Add"      -> bin Add
    "Subtract" -> bin Sub
    "Multiply" -> bin Mul
    "Divide"   -> bin Div
    "Power"    -> bin Pow
    "Negate"   -> unary Neg

    -- "Rational" special case (fraction form)
    "Rational" -> bin Div

    "Factorial"-> unary Factorial
    "Abs"      -> unary Abs

    -- Sqrt & Root
    "Sqrt"     -> unary Sqrt
    "Root"     -> tern Root

    -- Constants
    "Pi"           -> pure ConstantPi
    "ExponentialE" -> pure ConstantE

    -- Calculus, will be expeanded
    "Integral"     -> unary Integral
    "DefIntegral"  -> triple DefIntegral

    -- Limit
    "Limit" -> do
        case V.toList args of
            [String v, toVal, body] ->
                Limit (T.unpack v)
                    <$> fromMathJSON toVal
                    <*> fromMathJSON body
            [Array tuple, body] -> do
                (v, toVal) <- parseLimitTuple tuple
                Limit v
                    <$> fromMathJSON toVal
                    <*> fromMathJSON body
            _ -> fail "Invalid Limit form"
    -- Combinatorics
    "Combination" -> bin Combination
    "Permutation" -> bin Permutation
    "Variation"   -> bin Variation

    -- Prime notation
    "Prime" -> do
        [e, Number n] <- expect 2
        e' <- fromMathJSON e
        pure (Prime e' (round n))

    -- Unary functions: sin, cos, ln etc.
    _ | Just name <- lookup op functionTable ->
            unary (Func name)

    "Tuple" -> fail "Tuple is only allowed inside structured operators"
    "List" -> fail "Lists are not supported in expressions"
    "Set"  -> fail "Sets are not supported in expressions"
    _ -> fail $ "Unsupported operator: " ++ op

    --monad function definitions
  where
    -- expects one singular argument
    unary f = do
        [x] <- expect 1
        f <$> fromMathJSON x
    -- binary operation
    bin f = do
        [a,b] <- expect 2
        f <$> fromMathJSON a <*> fromMathJSON b
    -- Similar to  bin
    tern f = do
        [a,b] <- expect 2
        f <$> fromMathJSON a <*> fromMathJSON b
    -- Calculus exclusive
    triple f = do
        [a,b,c] <- expect 3
        f <$> fromMathJSON a <*> fromMathJSON b <*> fromMathJSON c
    -- anything else
    expect n =
        if V.length args == n
        then pure (V.toList args)
        else fail $ op ++ " expects " ++ show n ++ " arguments"
