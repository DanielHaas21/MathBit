{-# LANGUAGE DeriveFunctor #-}
module Struct.Expr where
 
-- ============================================================
-- 1. Core AST Type
-- ============================================================

data Expr
    = Var String                -- x, y, a, b, f, etc.
    | Num Double                -- numbers
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Neg Expr
    | Factorial Expr            -- n! or expression!
    | Abs Expr                  -- |x|
    | Func String Expr          -- sin(x), ln(x), etc.
    | Sqrt Expr                 -- \sqrt{x}
    | Root Expr Expr            -- \sqrt[n]{x}   as Root n x
    | Sigma Expr Expr Expr      -- \sum_{lower}^{upper} body
    | Product Expr Expr Expr    -- \prod_{lower}^{upper} body
    | Integral Expr             -- \int f(x) dx   (simple version)
    | DefIntegral Expr Expr Expr -- \int_a^b f(x) dx
    | Derivative String Expr    -- d/dx f(x)
    | Partial String Expr       -- ∂/∂x f(x)
    | Prime Expr Int            -- y', y'' , y'''
    | Limit String Expr Expr    -- \lim_{x→a} f(x)
    | Combination Expr Expr     -- C(n,k)
    | Permutation Expr Expr     -- P(n,k)
    | Variation Expr Expr       -- V(n,k)
    | ConstantPi                -- \pi
    | Angle                     -- \angle
    | Degree Expr               -- x^\circ
    deriving (Show, Eq)