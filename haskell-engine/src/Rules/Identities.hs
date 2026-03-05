{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Rules.Identities
  ( rules
  ) where

import Struct.Expr
import Struct.Rule
import Data.Ratio (denominator, numerator)

-- Classic algebraic identity rules.
-- All rules are Rewriting phase; they fire after structural normalisation.
-- Priority 6 keeps them just below diffOfSquaresProductN (7) so recognition
-- runs before re-expansion would undo it.

rules :: [Rule]
rules =
  [ squareOfSumN
  , squareOfDiffN
  , cubeOfSumN
  , cubeOfDiffN
  , diffOfSquaresProductN
  ]
  
-- Helpers

two :: Expr
two = Num (R 2)

three :: Expr
three = Num (R 3)

isIntN :: Integer -> Number -> Bool
isIntN k (R r) = r == fromInteger k && denominator r == 1
isIntN k (D d) = d == fromIntegral k

-- Extract (a, b) when an expression looks like  a - b  in any of the
-- forms the engine might store it:
--   Sub a b
--   Add a (Neg b)
--   Add a (Mul (Num -1) b)
asDiff :: Expr -> Maybe (Expr, Expr)
asDiff (Sub a b)                          = Just (a, b)
asDiff (Add a (Neg b))                    = Just (a, b)
asDiff (Add a (Mul (Num (R (-1))) b))     = Just (a, b)
asDiff (Add a (Mul (Num (D (-1.0))) b))   = Just (a, b)
asDiff _                                  = Nothing

-- (a+b)² = a² + 2ab + b²
squareOfSumN :: Rule
squareOfSumN = Rule
  "squareOfSumN"
  "Square of a sum: (a+b)² = a² + 2ab + b²"
  6 Rewriting $ \case
    Pow (Add a b) (Num n) | isIntN 2 n ->
      Just $
        Add
          (Add (Pow a two) (Mul two (Mul a b)))
          (Pow b two)
    _ -> Nothing

-- (a-b)² = a² - 2ab + b²
squareOfDiffN :: Rule
squareOfDiffN = Rule
  "squareOfDiffN"
  "Square of a difference: (a-b)² = a² - 2ab + b²"
  6 Rewriting $ \case
    Pow e (Num n) | isIntN 2 n, Just (a, b) <- asDiff e ->
      Just $
        Add
          (Add (Pow a two) (Mul (Num (R (-2))) (Mul a b)))
          (Pow b two)
    _ -> Nothing

-- (a+b)³ = a³ + 3a²b + 3ab² + b³
cubeOfSumN :: Rule
cubeOfSumN = Rule
  "cubeOfSumN"
  "Cube of a sum: (a+b)³ = a³ + 3a²b + 3ab² + b³"
  6 Rewriting $ \case
    Pow (Add a b) (Num n) | isIntN 3 n ->
      Just $
        Add
          (Add
            (Add (Pow a three) (Mul three (Mul (Pow a two) b)))
            (Mul three (Mul a (Pow b two))))
          (Pow b three)
    _ -> Nothing


-- (a-b)³ = a³ - 3a²b + 3ab² - b³
cubeOfDiffN :: Rule
cubeOfDiffN = Rule
  "cubeOfDiffN"
  "Cube of a difference: (a-b)³ = a³ - 3a²b + 3ab² - b³"
  6 Rewriting $ \case
    Pow e (Num n) | isIntN 3 n, Just (a, b) <- asDiff e ->
      Just $
        Add
          (Add
            (Add
              (Pow a three)
              (Mul (Num (R (-3))) (Mul (Pow a two) b)))
            (Mul three (Mul a (Pow b two))))
          (Mul (Num (R (-1))) (Pow b three))
    _ -> Nothing


-- (a+b)(a-b) = a² - b²
-- Recognises the product in any order and in any Sub/Add(Neg) form.
diffOfSquaresProductN :: Rule
diffOfSquaresProductN = Rule
  "diffOfSquaresProductN"
  "Product of a sum and a difference: (a+b)(a-b) = a² - b²"
  7 Rewriting $ \case
    Mul l r
      | Just (a, b) <- matchSumDiff l r -> result a b
      | Just (a, b) <- matchSumDiff r l -> result a b
    _ -> Nothing
  where
    result a b = Just $ Add (Pow a two) (Mul (Num (R (-1))) (Pow b two))

    -- Check that  p  is  (a+b)  and  q  is  (a-b)  for the same a, b.
    matchSumDiff (Add a1 b1) q
      | Just (a2, b2) <- asDiff q
      , a1 == a2, b1 == b2 = Just (a1, b1)
    matchSumDiff _ _ = Nothing
