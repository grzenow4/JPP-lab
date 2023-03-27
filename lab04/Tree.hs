import Control.Monad.Reader

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

type R a = Int -> a

renumber :: Tree a -> Tree Int
renumber tree = helper tree 0 where
    helper Empty        _ = Empty
    helper (Node x l r) n = Node n (helper l (n + 1)) (helper r (n + 1))

renumberRead :: Tree a -> Tree Int
renumberRead tree = helper tree 0 where
    helper :: Tree a -> R (Tree Int)
    helper Empty        = return Empty
    helper (Node x l r) = let fun = local (+1) . helper in
        Node <$> ask <*> fun l <*> fun  r

foo = Node 3 (Node 5 (Node 4 Empty Empty) Empty) (Node 0 Empty Empty)

-- >>> (toList $ renumberRead $ foo) == [0,1,2,1]
-- True

fromList :: [a] -> Tree a
fromList []     = Empty
fromList (x:xs) = Node x Empty (fromList xs)

toList :: Tree a -> [a]
toList Empty        = []
toList (Node x l r) = x : (toList l ++ toList r)

renumberTree :: Tree a -> Tree Int
renumberTree = error "todo"

-- >>> (toList $ renumberTree $ fromList "Learn Haskell") == [0..12]
-- True
