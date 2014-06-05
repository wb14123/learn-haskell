module Qsort(
    qsort
  , prop_idempotent
) where

qsort :: Ord a => [a] -> [a]
qsort []      = []
qsort (x: xs) = qsort lhs ++ [x] ++ rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs
