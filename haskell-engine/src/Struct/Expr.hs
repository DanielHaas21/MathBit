{-# LANGUAGE DeriveFunctor #-}

module Struct.Expr where
data Number
  = D Double      -- approximate, for non-exact evaluation
  | R Rational    -- exact, for fractions this covers things like powers and most division problems
  deriving (Eq, Ord, Show)

--Represent a lingua-franca for the entire parser, engine , Every expression follows this data type 
data Expr
    = Var String                -- x, y, a, b, f, etc.
    | Num Number                -- numbers split into Double and Rational
    | Add Expr Expr -- +
    | Sub Expr Expr -- Only used in cleanup/printing, internally always as Add + Neg or -1 *
    | Mul Expr Expr -- *  
    | Div Expr Expr -- / or Rational, used only in cleanup/printing, internally as Pow with negative exponent
    | Pow Expr Expr -- ^ 
    | Neg Expr -- Also only used in cleanup/printing, internally as Mul by -1
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
    | ConstantE                 -- e
    deriving (Show, Ord, Eq)