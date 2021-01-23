{- stack script
 --resolver lts-15.10
-}

-- to run -> stack 6_play.hs

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n -1)

-- `*` for non-negative numbers can be defined recursively
-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * n = m + (m * (n - 1))

-- product can be defined recursively
product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

-- lenth can be defined recursively
length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- reverse can be defined recursively
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = (reverse' xs) ++ [x]

-- Append operator `++` can also be defined recursively
-- Formalises the idea that two lists can be appended by
-- copying elements from the first list until it is exhausted,
-- at which point the second list is joined on at the end.
-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x : xs) ++ ys = x : (xs Main.++ ys)

-- a function that inserts a new element of any ordered type
-- into a sorted list to give another sorted list can be defined as follows:
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert' x ys

-- Using insert we can now define a function that implements insertion sort,
-- in which the empty list is already sorted,
--  and any non-empty list is sorted
-- by inserting its head into the list that results from sorting its tail:
isort' :: Ord a => [a] -> [a]
isort' [] = []
isort' (x : xs) = insert' x (isort' xs)

-- zip using recursion
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- drop using recrusion
drop' :: Integral b => b -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n -1) xs

-- MULTIPLE RECURSION

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n -2) + fib (n -1)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

-- MUTUAL RECURSION -- two for more functions are defined recursively in terms of each other
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

-- library function `init` that removes the last elment from a non-empty list
init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs

main :: IO ()
main = do
  print ("6_play.hs")
  print (fac 10)
  print (product' [10, 20, 30])
  print (length' [10 .. 50])
  print (reverse' [1 .. 5])
  --   print ([10 .. 14] Main.++ [1 .. 4])
  print (insert' 13 [x | x <- [10 .. 20], even x])
  print (isort' [7, 9, 1, 3, 0, 4, 6, 2, 8])
  print (zip' [11 .. 13] [1 .. 5])
  print (drop 4 [1 .. 10])
  print (drop 4 [1 .. 3])
  print (drop 0 [1 .. 10])
  print (qsort [1, 6, 8, 3, 4, 1, 3, 6, 8, 4, 0, 2, 6, 3, 7, 3, 2, 8, 3, 8, 6])
  print (even' 12)
  print (odd' 13)
  print (init' [1, 2, 3])
  print (init' [1])
