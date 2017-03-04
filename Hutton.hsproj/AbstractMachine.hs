module AbstractMachine where

{-

# Todo

Solve exercise 9 from ch. 8:

Extend the abstract machine to support the use of multiplication.

-}

-- Expressions
data Expr
  = Val Int
  | Add Expr Expr

{-
  Abstract Machine,
  being either in state expression evaluation
  or in state integer addition
-}

-- Control Stack
data Op
  = EVAL Expr
  | ADD Int
  
type Cont = [Op]

-- execute a control stack for an integer
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD m : c) n  = exec c (m + n)

-- controlled evaluation of an expression
eval :: Expr -> Cont -> Int
eval (Val n) c   = exec c n
eval (Add x y) c = eval x (EVAL y : c)

-- evaluate an expression
value :: Expr -> Int
value e = eval e []
