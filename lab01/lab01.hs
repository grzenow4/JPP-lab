import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.Set as Set
import Data.Set(Set)

sum :: Int -> Int
sum n = foldr (+) 0 [1..n]

scalarProd :: [Int] -> [Int] -> [Int]
scalarProd = zipWith (*)

triples :: Int -> [(Int,Int,Int)]
triples n = [(x, y, z) |
    x <- [1..n],
    y <- [1..n],
    z <- [1..n]]

triads :: Int -> [(Int,Int,Int)]
triads n = [(x, y, z) |
    x <- [1..n],
    y <- [1..n],
    z <- [1..n],
    x * x + y * y == z * z,
    x < y,
    gcd x y == 1]

incMaybe :: Maybe Int -> Maybe Int
incMaybe Nothing = Nothing
incMaybe (Just x) = Just (x + 1)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe _ Nothing = Nothing
addMaybe (Just x) (Just y) = Just (x + y)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

from :: Int -> [Int]
from n = (fib n):from (n + 1)

indexOf :: Char -> String -> Maybe Int
indexOf c s = findIndex s 0 where
    findIndex []     _ = Nothing
    findIndex (x:xs) n = if x == c then Just n else findIndex xs (n + 1)

positions :: Char -> String -> [Int]
positions c s = helper s 0 where
    helper []     _ = []
    helper (x:xs) n = let ps = helper xs (n + 1) in if c == x then (n:ps) else ps

showInt :: Int -> String
showInt = show

showIntLst :: [Int] -> String
showIntLst [] = "[]"
showIntLst (x:xs) = "[" ++ showInt x ++ helper xs ++ "]" where
    helper [] = ""
    helper (x:xs) = ", " ++ showInt x ++ helper xs

showLst :: (a -> String) -> [a] -> String
showLst _ [] = "[]"
showLst f (x:xs) = "[" ++ f x ++ (helper f xs) ++ "]" where
    helper _ [] = ""
    helper f (x:xs) = ", " ++ f x ++ helper f xs

charFreq :: String -> Map Char Int
charFreq = foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty

wordFreq :: String -> Map String Int
wordFreq s = foldr (\s m -> Map.insertWith (+) s 1 m) Map.empty (words s)

charWordFreq :: String -> (Map Char Int, Map String Int)
charWordFreq s = (charFreq s, wordFreq s)

charPresent :: String -> Set Char
charPresent = foldr (\c s -> Set.insert c s) Set.empty

charAbsent :: String -> Set Char
charAbsent s = Set.fromList [c | c <- ['a'..'z'], Set.notMember c (charPresent s)]

charPresentAbsent :: String -> (Set Char, Set Char)
charPresentAbsent s = (charPresent s, charAbsent s)

main = do
    putStrLn $ show $ charWordFreq "ala ola ula ala ela ula ala"
    putStrLn $ show $ charPresentAbsent "bcdfghjklmnpqrstvwxz"
