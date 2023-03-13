module OrderedList where

newtype OrderedList a = OL [a] deriving (Eq, Show)

instance Ord a => Semigroup (OrderedList a) where
    (<>) (OL xs) (OL ys) = OL (helper xs ys) where
        helper xs [] = xs
        helper [] ys = ys
        helper (x:xs) (y:ys)
            | x < y = x : helper xs (y:ys)
            | otherwise =  y : helper (x:xs) ys

instance Ord a => Monoid (OrderedList a) where
    mempty = OL []

nubOrdered :: Ord a => OrderedList a -> OrderedList a
nubOrdered (OL xs) = OL $ nub xs where
    nub [] = []
    nub (x:xs) = x : nub (filter (x/=) xs)
