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

-- This module contains the MathJSON -> AST parser, exports one top level function 

-- Top level dispatch function, based on data type each element is handled
-- Value as a type is aribtrary/ambigous
fromMathJSON :: Value -> Parser Expr
fromMathJSON v = case v of
    Array arr  -> parseArray arr -- All operators
    String s   -> pure (Var $ T.unpack s) -- Variables
    Number n   -> pure (Num $ realToFrac n)  -- Numbers
    _          -> fail "Invalid MathJSON expression"


-- Main parser for anything other than numbers and variables
parseArray :: Vector Value -> Parser Expr
parseArray arr
    | V.null arr = fail "Empty array"
    | otherwise =
        case V.head arr of
            String op -> parseOp (T.unpack op) (V.tail arr) -- In the parsers goes both string-text version and the array element
            _         -> fail "Array does not start with string operator"

-- Parses a limit if it has a tuple inside it, tuple is mainly used as a structure block 
parseLimitTuple :: Vector Value -> Parser (String, Value)
parseLimitTuple v = case V.toList v of
    ["Tuple", String var, toVal] -> -- Always consists of three arguments
        pure (T.unpack var, toVal)
    _ ->
        fail "Invalid limit tuple"

-- Function table for unary functions, that is any that are a form of f(x) 
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

-- Handles N-nary functions hence why the indefinite tuple notation (Expr -> Expr -> Expr) 
nary :: (Expr -> Expr -> Expr) -> Vector Value -> Parser Expr
nary f args
  | V.null args = fail "N-ary operator expects at least 1 argument"
  | V.length args == 1 = fromMathJSON (V.head args) -- single operand
  | otherwise = do
      let (x:xs) = V.toList args -- takes the tuple and creates a list 
      x' <- fromMathJSON x -- each element is parsed through the top level function 
      xs' <- mapM fromMathJSON xs -- mapped and folded
      pure $ foldl f x' xs'  -- we end with a left-associative array

-- Main operator parser
parseOp :: String -> Vector Value -> Parser Expr
parseOp op args = case op of
    -- Each operator accepts a different amount of arguments hence
    -- why each time a different function is used 


    -- Basic arithmetic
    "Add"      -> nary Add args
    "Multiply" -> nary Mul args
    "Divide"   -> bin Div
    "Power"    -> bin Pow
    "Negate"   -> unary Neg

    -- "Rational" special case, happens when a variable is in the numerator
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
    "Integrate"     -> unary Integral
    "DefIntegral"  -> triple DefIntegral

    -- Limit, accept three arguments, each is ran through the parser
    -- This monad accepts both a tuple or a normal limit 
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
    -- simple binary, since combinatorics arent conventionally simplified these are rarelly used
    "Combination" -> bin Combination
    "Permutation" -> bin Permutation
    "Variation"   -> bin Variation

    -- Prime notation
    -- Also rarely used 
    "Prime" -> do
        [e, Number n] <- expect 2
        e' <- fromMathJSON e
        pure (Prime e' (round n))

    -- Unary functions: sin, cos, ln etc.
    -- These have their helper, and are in a format Func (Args)
    _ | Just name <- lookup op functionTable ->
            unary (Func name)

    -- Some other oprators that frontend can produce none of these are supported
    "Tuple" -> fail "Tuple is only allowed inside structured operators"
    "List" -> fail "Lists are not supported in expressions"
    "Set"  -> fail "Sets are not supported in expressions"
    "ComplexInfinity" -> fail "Complex numbers arent supported"
    "PreDecrement" -> fail "No idea what this is but its not supported"
    "PreIncrement" -> fail "No idea what this is but its not supported"
    "Decrement" -> fail "No idea what this is but its not supported"
    _ -> fail $ "Unsupported operator: " ++ op

    
    -- all monadic helpers, each for (n) arguments
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
        -- fail
        expect n =
            if V.length args == n
            then pure (V.toList args)
            else fail $ op ++ " expects " ++ show n ++ " arguments" 

