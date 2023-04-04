data Exp 
  = EInt Integer        -- stała całkowita       
  | EAdd Exp Exp        -- e1 + e2
  | ESub Exp Exp        -- e1 - e2
  | EMul Exp Exp        -- e1 * e2
  | EVar String         -- zmienna
  | ELet String Exp Exp -- let var = e1 in e2
