module BitStringTransmit where

import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)



parity :: [Bit] -> Bit
parity xs
  | odd $ length $ filter (== 1) xs = 1
  | otherwise                       = 0

addParity :: [Bit] -> [Bit]
addParity xs = parity xs : xs

checkParity :: [Bit] -> [Bit]
checkParity [] = error "checkParity: empty list"
checkParity (x:xs)
  | x == parity xs = xs
  | otherwise      = error "checkParity: failed" 



make9 :: [Bit] -> [Bit]
make9 bits = take 9 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make9 . addParity . int2bin . ord)



chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9



channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode



unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

uf_int2bin :: Int -> [Bit]
uf_int2bin = unfold (== 0) (`mod` 2) (`div` 2)

uf_chop8 :: [Bit] -> [[Bit]]
uf_chop8 = unfold null (take 8) (drop 8)