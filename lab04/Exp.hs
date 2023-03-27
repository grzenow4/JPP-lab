import Control.Monad.Reader

type Var = String
type Env = [(Var, Int)]
type R a = Env -> a

data Exp = EInt Int
         | EOp  Op Exp Exp
         | EVar Var
         | ELet Var Exp Exp
data Op = OpAdd | OpSub | OpMul

locate :: Var -> Env -> Int
locate x []         = undefined
locate x ((y,v):ys) = if x == y then v
                      else locate x ys

hop :: Op -> (Int -> Int -> Int)
hop OpAdd = (+)
hop OpSub = (-)
hop OpMul = (*)

evalExp :: Exp -> Int
evalExp e = helper e [] where
      helper :: Exp -> R Int
      helper (EInt x)       = return x
      helper (EOp op e1 e2) = hop op <$> helper e1 <*> helper e2
      helper (EVar x)       = asks $ locate x
      helper (ELet x e1 e2) = do
            v <- helper e1
            local ((x,v):) (helper e2)

test :: Exp
test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9))
                      (EOp OpSub y (EInt 1)))
                (EOp OpMul x (EInt 3))
    where x = EVar "x"
          y = EVar "y"
