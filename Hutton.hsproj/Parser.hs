module Parser where


import Control.Applicative
import Data.Char
  

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

instance Functor Parser where
  fmap g p = P (\s ->
    case parse p s of
      []       -> []
      [(v, t)] -> [(g v, t)])

instance Applicative Parser where
  pure v = P (\s -> [(v, s)])
  pg <*> px = P (\s ->
    case parse pg s of
      []       -> []
      [(g, t)] -> parse (fmap g px) t)

instance Monad Parser where
  p >>= f = P (\s ->
    case parse p s of
      [] ->       []
      [(v, t)] -> parse (f v) t)



item :: Parser Char
item = P (\s ->
  case s of
    []     -> []
    (x:xs) -> [(x, xs)])

three :: Parser (Char, Char)
-- three = g <$> item <*> item <*> item
--  where g x y z = (x, z)
three = do
  x <- item
  item
  z <- item
  return (x, z)
