{-# LANGUAGE InstanceSigs #-}

module Tree where

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node x l r) = "(" ++ show l ++ ") " ++ show x ++ " (" ++ show r ++ ")"

instance Eq a => Eq (Tree a) where
    (==) Empty Empty = True
    (==) (Node x l1 r1) (Node y l2 r2) = x == y && l1 == l2 && r1 == r2
    (==) _ _ = False

toList :: Tree a -> [a]
toList Empty = []
toList (Node x l r) = toList l ++ [x] ++ toList r

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
    | x < y     = (Node y (insert x l) r)
    | otherwise = (Node y l (insert x r))

member :: (Ord a) => a -> Tree a -> Bool
member x Empty = False
member x (Node y l r)
    | x == y = True
    | x < y  = member x l
    | x > y  = member x r

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (\tree x -> insert x tree) Empty

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
