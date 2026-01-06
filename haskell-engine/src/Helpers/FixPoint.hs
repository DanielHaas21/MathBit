{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Helpers.FixPoint (fixpoint) where


-- Function that repeatedly applies a function until the result no longer changes.
-- Returns the fixed point: the first value @y@ such that @f y == y@.
-- Requires an 'Eq a' instance to compare successive results for convergence.
-- Useful for iterative computations like normalization or simplification passes
-- that converge after repeated application of the same transformation.
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x =
  let x' = f x
  in if x' == x then x else fixpoint f x'