{-# LANGUAGE InstanceSigs #-}

import Exp
import MyMaybe
import OrderedList
import Prelude hiding(Either(..))
import ReadInts
import Tree

data Either a b = Left a | Right b

instance Functor (Either e) where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left x) = Left x
    fmap f (Right x) = Right $ f x

elimMaybe :: c -> (a -> c) -> Maybe a -> c
elimMaybe x _ Nothing = x
elimMaybe _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just $ f x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

elimEither :: (a  -> c) -> (b -> c) -> Either a b -> c
elimEither f _ (Left x) = f x
elimEither _ g (Right x) = g x

mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither f _ (Left x) = Left $ f x
mapEither _ g (Right x) = Right $ g x

mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
mapRight _ (Left x) = Left x
mapRight f (Right x) = Right $ f x

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

reverseRight :: Either e [a] -> Either e [a]
reverseRight (Left x) = Left x
reverseRight (Right xs) = Right $ reverse xs

reverseRight2 :: Either e [a] -> Either e [a]
reverseRight2 = fmap reverse
