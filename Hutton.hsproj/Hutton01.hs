module Hutton01 where

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [ y | y <- xs, y <= x ]
    larger = [ z | z <- xs, z > x ]
