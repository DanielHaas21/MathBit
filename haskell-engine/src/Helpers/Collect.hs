{-# LANGUAGE DataKinds #-}
module Helpers.Collect (collectAdd, collectMul) where

import Struct.Expr

-- Collect all Additive expressions into an array via merging
-- e.g. a + b + c -> [a, b, c]
collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e         = [e]

-- Collect all Multiplicable expressions into an array via merging
-- e.g. a * b * c -> [a, b, c]
collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e         = [e]