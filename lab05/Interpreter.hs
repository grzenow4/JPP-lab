module Interpreter where

import Control.Monad.Reader
import Data.Map as Map
import Exp.AbsExp

type Env = Map Ident Integer
type R a = Env -> a

getVar :: Ident -> Env -> Integer
getVar x env = case Map.lookup x env of
    Just v -> v
    Nothing -> error "Variable not found"

interpret :: Exp -> R Integer
interpret (EInt n)       = return n
interpret (EAdd e1 e2)   = (+) <$> interpret e1 <*> interpret e2
interpret (ESub e1 e2)   = (-) <$> interpret e1 <*> interpret e2
interpret (EMul e1 e2)   = (*) <$> interpret e1 <*> interpret e2
interpret (EVar x)       = asks (getVar x)
interpret (ELet x e1 e2) = do
    v <- interpret e1
    local (Map.insert x v) (interpret e2)
