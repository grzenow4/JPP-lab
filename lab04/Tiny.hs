import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Map as Map

type Var = String
type Loc = Int
type Env = Map Var Loc
type Store = Map Loc Int
type IM a = ExceptT String (StateT Store (Reader Env)) a

data Decl = DVar Var Exp
data Stmt = Skip
          | Var := Exp
          | Stmt :> Stmt
          | IfElse BExp Stmt Stmt
          | While BExp Stmt
          | Block [Decl] Stmt
data Exp = EInt Int
         | EVar Var
         | EAdd Exp Exp
         | ESub Exp Exp
         | EMul Exp Exp
         | EDiv Exp Exp
data BExp = Tt | Ff | Not BExp
          | Exp :== Exp | Exp :/= Exp
          | Exp :>> Exp | Exp :>= Exp
          | Exp :<< Exp | Exp :<= Exp

instance Num Exp where
    (+) = EAdd
    (-) = ESub
    (*) = EMul
    abs = undefined
    signum = undefined
    fromInteger = EInt . fromInteger

alloc :: Store -> Loc
alloc = Map.size

writeLoc :: Loc -> Int -> IM ()
writeLoc l v = modify (Map.insert l v)

getLoc :: Var -> IM Loc
getLoc x = do
    env <- ask
    case Map.lookup x env of
        Just l -> return l
        Nothing -> except $ Left (x ++ " not found in the enviornemnt")

getVal :: Loc -> IM Int
getVal l = do
    s <- get
    case Map.lookup l s of
        Just v -> return v
        Nothing -> except $ Left ("Loc not found in the store")

exec :: Stmt -> IM ()
exec Skip             = return ()
exec (x := e)         = do { l <- getLoc x; evalE e >>= writeLoc l }
exec (s1 :> s2)       = exec s1 >> exec s2
exec (IfElse b s1 s2) = do { f <- evalB b; if f then exec s1 else exec s2 }
exec (While b s)      = do
    f <- evalB b
    if f then exec (s :> While b s) else exec Skip
exec (Block [] s)     = exec s
exec (Block ((DVar x e):ds) s) = do
    v <- evalE e
    l <- gets alloc
    modify (Map.insert l v)
    local (Map.insert x l) (exec (Block ds s))

evalE :: Exp -> IM Int
evalE (EInt n)     = return n
evalE (EVar x)     = getLoc x >>= getVal
evalE (EAdd e1 e2) = (+) <$> evalE e1 <*> evalE e2
evalE (ESub e1 e2) = (-) <$> evalE e1 <*> evalE e2
evalE (EMul e1 e2) = (*) <$> evalE e1 <*> evalE e2
evalE (EDiv e1 e2) = do
    x <- evalE e1
    y <- evalE e2
    if y == 0 then except $ Left ("Cannot divide by 0")
              else return (x `div` y)

evalB :: BExp -> IM Bool
evalB Tt          = return True
evalB Ff          = return False
evalB (Not b)     = (not) <$> evalB b
evalB (e1 :== e2) = (==) <$> evalE e1 <*> evalE e2
evalB (e1 :/= e2) = (/=) <$> evalE e1 <*> evalE e2
evalB (e1 :>> e2) = (>) <$> evalE e1 <*> evalE e2
evalB (e1 :>= e2) = (>=) <$> evalE e1 <*> evalE e2
evalB (e1 :<< e2) = (<) <$> evalE e1 <*> evalE e2
evalB (e1 :<= e2) = (<=) <$> evalE e1 <*> evalE e2

run :: Stmt -> (Either String (), Store)
run = runIM . exec

runIM :: IM a -> (Either String a, Store)
runIM = runEnvR . runStoreS . runExceptT

runEnvR :: Reader Env a -> a
runEnvR r = runReader r Map.empty

runStoreS :: StateT Store m a -> m (a, Store)
runStoreS s = runStateT s Map.empty

-- test
-- x := 42
-- y := 69
-- if x != y then
--     x := x * 10
-- else
--     y := y * 10
-- >>> x = 420, y = 69
test :: Stmt
test = Block [DVar "x" 42, DVar "y" 69] (
    IfElse (EVar "x" :/= EVar "y")
           ("x" := (EVar "x" * 10))
           ("y" := (EVar "y" * 10))
    )

-- test2 n
-- x := 1;
-- y := 0;
-- while x <= n do
--     y := y + x;
--     x := x + 1;
-- end
-- >>> y = n * (n + 1) / 2
test2 :: Int -> Stmt
test2 n = Block [DVar "x" 1, DVar "y" 0] (
    While (EVar "x" :<= EInt n) (
        ("y" := (EVar "y" + EVar "x")) :>
        ("x" := (EVar "x" + 1))
    ))

-- test3 n
-- s := 1; i := 1;
-- while i < n do
--     k := 1; l := 0; p := 1;
--     while p < i do
--         k := k + 1;
--         if l < p then
--             l := l + 1;
--             p := 1;
--         else
--             p := p + 1;
--     s := s + 6 * k + p - l;
--     i := i + 1;
-- >>> s = n ^ 3
test3 :: Int -> Stmt
test3 n = Block [DVar "s" 1, DVar "i" 1] (
    While (EVar "i" :<< EInt n) (
        Block [DVar "k" 1, DVar "l" 0, DVar "p" 1] (
            (While (EVar "p" :<< EVar "i") (
                ("k" := (EVar "k" + 1)) :>
                (IfElse (EVar "l" :<< EVar "p")
                        (("l" := (EVar "l" + 1)) :> ("p" := 1))
                        ("p" := (EVar "p" + 1)))
            )) :>
            ("s" := (EVar "s" + 6 * EVar "k" + EVar "p" - EVar "l")) :>
            ("i" := (EVar "i" + 1))
        )
    ))
