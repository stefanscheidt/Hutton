module Hutton02 where

n :: Int
n = a `div` (length xs)
  where
    a = 10
    xs = [1,2,3,4,5]
    
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myLast :: [a] -> a
myLast xs = head (myReverse xs)

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x : myInit xs
