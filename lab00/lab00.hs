countdown :: Int -> [Int]
countdown 0 = [0]
countdown n = n:countdown (n - 1)

collatz :: Integer -> [Integer]
collatz n | n == 1       = [1]
          | mod n 2 == 1 = n : collatz (3 * n + 1)
          | otherwise    = n : collatz (div n 2)

myhead :: [a] -> a
myhead [] = error "List cannot be empty"
myhead (x:xs) = x

mytail :: [a] -> [a]
mytail [] = error "List cannot be empty"
mytail (x:xs) = xs

(+++) :: [a] -> [a] -> [a]
(+++) [] y = y
(+++) (x:xs) y = x : (+++) xs y

mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 _ = []
mytake n (x:xs) = x : mytake (n - 1) xs

mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop 0 x = x
mydrop n (x:xs) = mydrop (n - 1) xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) = if f x then x : myfilter f xs else myfilter f xs

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = (f x) : mymap f xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

suffs :: [a] -> [[a]]
suffs [] = [[]]
suffs (x:xs) = (x:xs) : inits xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : [x:ys | ys <- inits xs]

partitions :: [a] -> [([a], [a])]
partitions [] = [([], [])]
partitions (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- partitions xs]

inserts :: a -> [a] -> [[a]]
inserts x xs = [ys ++ [x] ++ zs | (ys, zs) <- partitions xs]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [inserts x ys | ys <- permutations xs]

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:nub (filter (x /=) xs)

incAll :: [[Int]] -> [[Int]]
incAll = map $ map (+1)

factor :: Int -> Int
factor n = if n < 0 then 1 else foldr (*) 1 [1..n]

myconcat2 :: [[a]] -> [a]
myconcat2 = foldr (++) []
