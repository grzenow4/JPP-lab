module ReadInts where
import Data.Char

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

mapRight ::  (b1 -> b2) -> Either a b1 -> Either a b2
mapRight _ (Left x) = Left x
mapRight f (Right x) = Right $ f x

readInts :: String -> [Int]
readInts s = map read $ filter (all isDigit) (words s)

readInts2 :: String -> Either String [Int]
readInts2 s = helper (words s) where
    helper [] = Right []
    helper (x:xs) = if all isDigit x
                    then mapRight (read x :) $ helper xs
                    else Left $ "Not a number: " ++ x

sumInts :: String -> String
sumInts s = fromEither $ helper (words s) 0 where
    helper [] n = Right $ show n
    helper (x:xs) n = if all isDigit x
                      then helper xs (read x + n)
                      else Left $ "Not a number: " ++ x
