module AbstractMachine where

-- Expressions
data Expr
  = Val Int
  | Add Expr Expr
  | Mult Expr Expr

{-
  Abstract Machine,
  being either in state expression evaluation
  or in state operand application
-}

-- Operand
type Oper = (Int -> Int -> Int)

-- Operation
data Op
  = EVAL Oper Expr
  | APLY Oper Int
  
-- Control Stack
type Cont = [Op]

-- execute a control stack for an integer
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL p y : c) n = eval y (APLY p n : c)
exec (APLY p m : c) n  = exec c (p m n)

-- controlled evaluation of an expression
eval :: Expr -> Cont -> Int
eval (Val n) c   = exec c n
eval (Add x y) c = eval x (EVAL (+) y : c)
eval (Mult x y) c = eval x (EVAL (*) y : c)

-- evaluate an expression
value :: Expr -> Int
value e = eval e []
