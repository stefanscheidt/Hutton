module Hutton05 where

factors :: Int -> [Int]
factors n = [k | k <- [1..n], n `mod` k == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [k | k <- [1..n], isPrime k]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

isPerfect :: Int -> Bool
isPerfect k = sum (tail $ reverse $ factors k) == k

perfects :: Int -> [Int]
perfects n = [k | k <- [1..n-1], isPerfect k]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
