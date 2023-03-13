module Exp where

data Exp 
  = EInt Int             -- stała całkowita       
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2 
  deriving Eq

instance Show Exp where
    show (EInt x)       = show x
    show (EAdd e1 e2)   = show e1 ++ " + " ++ show e2
    show (ESub e1 e2)   = show e1 ++ " - " ++ show e2
    show (EMul e1 e2)   = show e1 ++ " * " ++ show e2
    show (EVar x)       = x
    show (ELet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

instance Num Exp where
    (+) e1 e2 = simpl (EAdd e1 e2)
    (*) e1 e2 = simpl (EMul e1 e2)
    abs = undefined
    signum = undefined
    fromInteger = EInt . fromInteger
    negate = simpl . (EMul $ EInt (-1))

simpl :: Exp -> Exp
simpl (EAdd e 0)     = simpl e
simpl (EAdd 0 e)     = simpl e
simpl (ESub e 0)     = simpl e
simpl (ESub 0 e)     = negate $ simpl e
simpl (EMul e 1)     = simpl e
simpl (EMul 1 e)     = simpl e
simpl (EMul _ 0)     = 0
simpl (EMul 0 _)     = 0
simpl (ELet x e1 e2) = ELet x (simpl e1) (simpl e2)
simpl e              = e
