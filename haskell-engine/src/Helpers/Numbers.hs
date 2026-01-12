module Helpers.Numbers (isZero, isOne, addNum, mulNum, divNum, powNum, negNum, absNum, numberLtZero) where

import Struct.Expr
import Data.Ratio (numerator, denominator)

-- This file contains various helper functions for dealing with numbers in both
-- rational and decimal form within expressions. These include arithmetic operations
-- such as addition, multiplication, negation, comparison, etc.


-- determine if a Number R D is zero or one
isZero, isOne :: Number -> Bool
isZero (R x) = x == 0
isZero (D x) = x == 0
isOne  (R x) = x == 1
isOne  (D x) = x == 1

-- add up two R or D numbers
addNum :: Number -> Number -> Number
addNum (R x) (R y) = R (x + y)
addNum (D x) (D y) = D (x + y)
addNum (R x) (D y) = D (fromRational x + y)
addNum (D x) (R y) = D (x + fromRational y)

-- multiply, divide and power two R or D numbers
mulNum, divNum, powNum :: Number -> Number -> Number

mulNum (R x) (R y) = R (x * y)
mulNum (D x) (D y) = D (x * y)
mulNum (R x) (D y) = D (fromRational x * y)
mulNum (D x) (R y) = D (x * fromRational y)

divNum (R x) (R y) = R (x / y)
divNum (D x) (D y) = D (x / y)
divNum (R x) (D y) = D (fromRational x / y)
divNum (D x) (R y) = D (x / fromRational y)

powNum (R x) (R y)
  | denominator y == 1 = R (x ^^ numerator y) -- integer powers exact
  | otherwise          = D (fromRational x ** fromRational y)
powNum (D x) (D y) = D (x ** y)
powNum (R x) (D y) = D (fromRational x ** y)
powNum (D x) (R y) = D (x ** fromRational y)    

-- negate a number
negNum :: Number -> Number
negNum (R x) = R (-x)
negNum (D x) = D (-x)

-- absolute value of a number
absNum :: Number -> Number
absNum (R x) = R (abs x)
absNum (D x) = D (abs x)

-- check if a number is less than zero
numberLtZero :: Number -> Bool
numberLtZero (R r) = r < 0
numberLtZero (D d) = d < 0