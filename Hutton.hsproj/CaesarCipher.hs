module CaesarCipher where
  
import Data.Char

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x' == x]

lowers :: String -> Int
lowers cs = length [c | c <- cs, c >= 'a' && c <= 'z']

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c' == c]

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2char $ (char2int c + n) `mod` 26
  | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

enFreqs :: [Float]
enFreqs = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
  0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
  6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 1.0]
  
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z']]
  where n = lowers cs
  
chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum [(x - y)^1/y | (x,y) <- zip xs ys]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack cs = encode (-factor) cs
  where
    csFreqs = freqs cs
    chitab = [chisqr (rotate n csFreqs) enFreqs | n <- [0..26]]
    factor = head (positions (minimum chitab) chitab)