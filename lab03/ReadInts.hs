import Data.Char
import Text.Read

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

helper :: [String] -> Either String [Int]
helper [] = Right []
helper (x:xs) = (:) <$> readEither x <*> helper xs

readInts2 :: String -> Either String [Int]
readInts2 = helper . words

helper2 :: [String] -> Either String Int
helper2 [] = Right 0
helper2 (x:xs) = do
    y <- readEither x
    z <- helper2 xs
    return $ y + z

sumInts :: String -> String
sumInts = fromEither . fmap show . helper2 . words
