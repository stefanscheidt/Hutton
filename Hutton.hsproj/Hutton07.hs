module Hutton07 where

all_ :: (a -> Bool) -> [a] -> Bool
all_ p = and . map p

any_ :: (a -> Bool) -> [a] -> Bool
any_ p = or . map p

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ _ [] = []
takeWhile_ p (x:xs)
  | p x       = x : takeWhile_ p xs
  | otherwise = []

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ _ [] = []
dropWhile_ p (x:xs)
  | p x       = dropWhile_ p xs
  | otherwise = x:xs
  
map_ :: (a -> b) -> [a] -> [b]
map_ f = foldr (\x ys -> f x : ys) []

filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\acc n -> 10 * acc + n) 0

curry_ :: ((a,b) -> c) -> (a -> b -> c)
curry_ f x y = f (x, y)

uncurry_ :: (a -> b -> c) -> ((a,b) -> c)
uncurry_ f (x,y) = f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

uf_map :: (a -> b) -> [a] -> [b]
uf_map f = unfold null (f . head) tail

uf_iterate :: (a -> a) -> a -> [a]
uf_iterate f = unfold (\_ -> False) f f