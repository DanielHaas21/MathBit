{-# LANGUAGE DataKinds #-}
module Helpers.Partition (partitionNums) where
  
import Struct.Expr

-- Helper to partition numeric and non-numeric expressions into two seperate index assocaited arrays 
-- e.g. [2, x, 3, y] -> ([2,3], [x,y])
partitionNums :: [Expr] -> ([Number], [Expr])
partitionNums [] = ([], [])
partitionNums (Num n : xs) =
  let (ns, es) = partitionNums xs in (n:ns, es) -- Numeric case
partitionNums (e:xs) =
  let (ns, es) = partitionNums xs in (ns, e:es) -- Expression case