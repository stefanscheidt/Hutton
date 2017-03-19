module CountdownProblem where

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

-- arithmetic operands
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- valid operations
-- exploiting algebraic properties
-- rules out "equal expressions" to optimize performance
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0

-- application of an operand
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Expressions
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

-- All integer values of an expressions
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- evaluate an expression
-- "Failure within eval could be handled by using Maybe,
-- but we prefer list because the comprehension notation
-- provides a convenient way to define eval."
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]



-- combinatorial helpers

-- all subsequences of a list
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

-- all ways to insert an element in a list
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) 

-- all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- all permutations of all subsequences
-- aka all possible ways of selecting zero or more elements in any order from a list
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- alternative way to define choices (Ex. 9.1)
choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]

-- Ex. 9.2
rmFst :: Eq a => a -> [a] -> [a]
rmFst x []     = []
rmFst x (y:ys) = if x == y then ys else y : rmFst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _  = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (rmFst x ys)

-- a solution for the countdown problem?
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]



-- find solutions, brute force

-- all ways of splitting a list into two non-empty sublists
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- all expressions for a list of values
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [(Val n)]
exprs ns  = [e | 
    (ls,rs) <- split ns,
    l       <- exprs ls,
    r       <- exprs rs,
    e       <- combine l r]
  where
    combine l r =
      [App o l r | o <- [Add,Sub,Mul,Div]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ms <- choices ns, e <- exprs ms, eval e == [n]]



-- find solutions, combining generation and evaluation

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res |
    (ls, rs) <- split ns,
    lx       <- results ls,
    ry       <- results rs,
    res      <- combine lx ry]
  where
    combine (l, x) (r, y) =
      [(App o l r, apply o x y) | o <- [Add,Sub,Mul,Div], valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ms <- choices ns, (e, m) <- results ms, m == n]
