module Hutton08 where

-- recursive definition of natural numbers
data Nat
  = Zero
  | Succ Nat
  deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n     = Zero
mult (Succ m) n = add n (mult m n)

-- Trees
data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)

-- for search trees
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) =
  case (compare x y) of
    LT -> occurs x l
    EQ -> True
    GT -> occurs x r
    
-- binary trees
data BTree a
  = BLeaf a
  | BNode (BTree a) (BTree a)
  deriving Show

balanced :: BTree a -> Bool
balanced (BLeaf _) = True
balanced (BNode l r)
  = abs (leaves l - leaves r) <= 1
    && balanced l
    && balanced r
  where
    leaves (BLeaf _)   = 1
    leaves (BNode l r) = leaves l + leaves r
    
balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs = BNode (balance (fst halves)) (balance (snd halves))
  where
    len = (length xs) `div` 2
    halves = splitAt len xs

-- Expressions
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)   = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)


