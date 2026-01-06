{-# LANGUAGE DataKinds #-}
module Helpers.Collect (collectAdd, collectMul) where

import Struct.Expr

-- Collect all Additive expressions into an array via merging
collectAdd :: Expr -> [Expr]
collectAdd (Add x y) = collectAdd x ++ collectAdd y
collectAdd e         = [e]

-- Collect all Multiplicable expressions into an array via merging
collectMul :: Expr -> [Expr]
collectMul (Mul x y) = collectMul x ++ collectMul y
collectMul e         = [e]