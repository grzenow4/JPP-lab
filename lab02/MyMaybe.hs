module MyMaybe where

data MyMaybe a = MyNothing | MyJust a

instance Show a => Show (MyMaybe a) where
    showsPrec _ MyNothing = showString "MyNothing"
    showsPrec p (MyJust x) = showParen (p > 0) $ showString "MyJust " . showsPrec 1 x
