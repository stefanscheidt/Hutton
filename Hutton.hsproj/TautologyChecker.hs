module TautologyChecker where

{--

# Todo

* Let find return Maybe v or ...
* ... use Data.Map instead of Assoc

--}

-- Propositions
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  
-- Lookup tables, used for Substitutions
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k ]

-- Substitutions
type Subst = Assoc Char Bool

-- evaluate a Proposition for a given Substitution
-- makes use of the fact that b equiv c iff b <= c for b, c :: Bool
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

-- all variables in a proposition
vars :: Prop -> [Char]
vars p = rmdups $ vars' p
  where
    vars' (Const _) = []
    vars' (Var x)   = [x]
    vars' (Not p)   = vars' p
    vars' (And p q) = vars' p ++ vars' q
    vars' (Imply p q) = vars' p ++ vars' q
    rmdups []     = []
    rmdups (x:xs) = x : [x' | x' <- xs, x' /= x]

-- all combinations of n bools
-- used to create all possible substs for a given list of vars
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n - 1)

-- all possible substs for a prop
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = vars p
  
-- finally, the tautology checker
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
