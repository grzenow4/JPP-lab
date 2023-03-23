import Control.Monad

data Exp = Val Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Var String
         | Let String Exp Exp
type Env = [(String, Int)]

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

eval :: Exp -> Maybe Int
eval (Val n)   = return n
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Sub x y) = (-) <$> eval x <*> eval y
eval (Mul x y) = (*) <$> eval x <*> eval y
eval (Div x y) = join $ safediv <$> eval x <*> eval y

evalList :: [Exp] -> Maybe [Int]
evalList []     = return []
evalList (x:xs) = (:) <$> eval x <*> evalList xs

evalList' :: [Exp] -> [Maybe Int]
evalList' = map eval

safediv2 :: Int -> Int -> Either String Int
safediv2 _ 0 = Left "Division by zero"
safediv2 x y = Right (div x y)

eval2 :: Exp -> Either String Int
eval2 (Val n)   = return n
eval2 (Add x y) = (+) <$> eval2 x <*> eval2 y
eval2 (Sub x y) = (-) <$> eval2 x <*> eval2 y
eval2 (Mul x y) = (*) <$> eval2 x <*> eval2 y
eval2 (Div x y) = join $ safediv2 <$> eval2 x <*> eval2 y

evalList2 :: [Exp] -> Either String [Int]
evalList2 []     = return []
evalList2 (x:xs) = (:) <$> eval2 x <*> evalList2 xs

evalList2' :: [Exp] -> [Either String Int]
evalList2' = map eval2

varValue :: Env -> String -> Either String Int
varValue []     _ = Left "Variable not found in environment."
varValue (x:xs) y = if fst x == y
                    then Right $ snd x
                    else varValue xs y

addVar :: Env -> (String, Int) -> Either String Env
addVar []     y = Right [y]
addVar (x:xs) y = if fst x == fst y
                  then Left "Variable already in environment"
                  else (:) <$> return x <*> addVar xs y

eval3 :: Env -> Exp -> Either String Int
eval3 rho (Val n)       = return n
eval3 rho (Add e1 e2)   = (+) <$> eval3 rho e1 <*> eval3 rho e2
eval3 rho (Sub e1 e2)   = (-) <$> eval3 rho e1 <*> eval3 rho e2
eval3 rho (Mul e1 e2)   = (*) <$> eval3 rho e1 <*> eval3 rho e2
eval3 rho (Div e1 e2)   = join $ safediv2 <$> eval3 rho e1 <*> eval3 rho e2
eval3 rho (Var x)       = varValue rho x
eval3 rho (Let x e1 e2) = do
    val <- eval3 rho e1
    rho' <- addVar rho (x, val)
    eval3 rho' e2
