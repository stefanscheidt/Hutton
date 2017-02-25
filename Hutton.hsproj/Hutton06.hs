module Hutton06 where

fac :: Int -> Int
fac n
  | n < 0     = error "fac not defined for negative numbers"
  | n == 0    = 1
  | otherwise = n * fac (n - 1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(§) :: Int -> Int -> Int
n § m
  | m <= 0    = error "§ not defined for negative numbers"
  | m == 1     = n
  | otherwise = n * (n § (m - 1))

euclid :: Int -> Int -> Int
euclid k l
  | k == l    = l
  | k < l     = euclid k (l - k)
  | otherwise = euclid l (k - l)
  
rec_and :: [Bool] -> Bool
rec_and [] = True
rec_and (x:xs) = x && (rec_and xs)

rec_concat :: [[a]] -> [a]
rec_concat [] = []
rec_concat (x:xs) = x ++ (rec_concat xs)

rec_replicate :: Int -> a -> [a]
rec_replicate n x
  | n <= 0    = []
  | otherwise = [x] ++ (rec_replicate (n - 1) x)
  
rec_nth :: Int -> [a] -> a
rec_nth n xs
  | n < 0          = error "rec_nth: negative index"
  | n >= length xs = error "rec_nth: index too large"
  | n == 0         = head xs
  | otherwise      = rec_nth (n - 1) (drop 1 xs)
  
rec_elem :: Eq a => a -> [a] -> Bool
rec_elem x [] = False
rec_elem x (y:ys)
  | x == y    = True
  | otherwise = rec_elem x ys
  
rec_merge :: Ord a => [a] -> [a] -> [a]
rec_merge xs [] = xs
rec_merge [] ys = ys
rec_merge (x:xs) (y:ys)
  | x <= y    = x : (rec_merge xs (y:ys))
  | otherwise = y : (rec_merge (x:xs) ys)

splitpoint :: Ord a => [a] -> (Int, Int)
splitpoint xs = (k, l)
  where
    k = length xs `div` 2
    l = k + length xs `rem` 2

halve :: Ord a => [a] -> ([a], [a])
halve xs = (take k xs, drop k xs)
  where
    k = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = rec_merge (msort $ fst $ parts) (msort $ snd $ parts)
  where parts = halve xs