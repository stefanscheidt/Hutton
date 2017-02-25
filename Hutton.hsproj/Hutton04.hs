module Hutton04 where

myHalve :: [a] -> ([a], [a])
myHalve xs =
  let
    len = (length xs) `div` 2
  in
    splitAt len xs

myThird1 :: [a] -> a
myThird1 = head . tail . tail

myThird2 :: [a] -> a
myThird2 xs = xs !! 2

myThird3 :: [a] -> a
myThird3 (_ : _ : x : _) = x

myTail1 :: [a] -> [a]
myTail1 xs =
  if null xs then
    []
  else
    tail xs 
    
myTail2 :: [a] -> [a]
myTail2 xs
  | null xs   = []
  | otherwise = tail xs

myTail3 :: [a] -> [a]
myTail3 [] = []
myTail3 (_:xs) = xs

luhnDouble :: Int -> Int
luhnDouble n
  | double > 9 = double - 9
  | otherwise  = double
  where double = n + n
  
luhn4 :: Int -> Int -> Int -> Int -> Bool
luhn4 a b c d
  | s `mod` 10 == 0 = True
  | otherwise       = False
  where s = luhnDouble a + b + luhnDouble c + d

-- from chapter 7

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = map (\(x,i) -> if (odd i) then (f x) else (g x)) (zip xs [1..])

int2digits :: Int -> [Int]
int2digits = reverse . digits
  where 
    digits 0 = []
    digits n = n `mod` 10 : digits (n `div` 10)

luhn :: Int -> Bool
luhn x
  | s `mod` 10 == 0 = True
  | otherwise       = False
  where s = sum $ altMap luhnDouble id (int2digits x)