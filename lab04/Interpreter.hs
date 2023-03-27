module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map as Map
import Tiny.AbsTiny

type Var = String
type Loc = Int
type Env = Map Var Loc
type Store = Map Loc Integer
type IM a = ExceptT String (StateT Store (Reader Env)) a

alloc :: Store -> Loc
alloc = Map.size

writeLoc :: Loc -> Integer -> IM ()
writeLoc l v = modify (Map.insert l v)

getLoc :: Var -> IM Loc
getLoc x = do
    env <- ask
    case Map.lookup x env of
        Just l -> return l
        Nothing -> except $ Left ("Variable not found in the enviornemnt")

getVal :: Loc -> IM Integer
getVal l = do
    s <- get
    case Map.lookup l s of
        Just v -> return v
        Nothing -> except $ Left ("Loc not found in the store")

aOp :: AddOp -> Integer -> Integer -> Integer
aOp Plus = (+)
aOp Minus = (-)

mOp :: MulOp -> Integer -> Integer -> Integer
mOp Times = (*)
mOp Div = div
mOp Mod = mod

rOp :: RelOp -> Integer -> Integer -> Bool
rOp Tiny.AbsTiny.LT = (<)
rOp Tiny.AbsTiny.LE = (<=)
rOp Tiny.AbsTiny.GT = (>)
rOp Tiny.AbsTiny.GE = (>=)
rOp Tiny.AbsTiny.EQ = (==)
rOp Tiny.AbsTiny.NE = (/=)

runP :: Program -> IM ()
runP (Prog s) = exec s

exec :: Stmt -> IM ()
exec SSkip              = return ()
exec (SVar (Ident x) e) = do { l <- getLoc x; evalE e >>= writeLoc l }
exec (SSem s1 s2)       = exec s1 >> exec s2
exec (SIfElse b s1 s2)  = do { f <- evalB b; if f then exec s1 else exec s2 }
exec (SWhile b s)       = exec $ SIfElse b (SSem s (SWhile b s)) SSkip
exec (SBlock [] s)      = exec s
exec (SBlock ((DVar (Ident x) e):ds) s) = do
    v <- evalE e
    l <- gets alloc
    modify (Map.insert l v)
    local (Map.insert x l) (exec (SBlock ds s))

evalE :: Exp -> IM Integer
evalE (EInt n)         = return n
evalE (EVar (Ident x)) = getLoc x >>= getVal
evalE (ENeg e)         = evalE e >>= return . negate
evalE (EAdd e1 op e2)  = aOp op <$> evalE e1 <*> evalE e2
evalE (EMul e1 op e2)  = mOp op <$> evalE e1 <*> evalE e2

evalB :: BExp -> IM Bool
evalB BT              = return True
evalB BF              = return False
evalB (BNeg b)        = not <$> evalB b
evalB (BOr  b1 b2)    = (||) <$> evalB b1 <*> evalB b2
evalB (BAnd b1 b2)    = (&&) <$> evalB b1 <*> evalB b2
evalB (BRel e1 op e2) = rOp op <$> evalE e1 <*> evalE e2

run :: Program -> (Either String (), Store)
run = runIM . runP

runIM :: IM a -> (Either String a, Store)
runIM = runEnvR . runStoreS . runExceptT

runEnvR :: Reader Env a -> a
runEnvR r = runReader r Map.empty

runStoreS :: StateT Store m a -> m (a, Store)
runStoreS s = runStateT s Map.empty
