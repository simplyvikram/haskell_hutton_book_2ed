{- stack script
 --resolver lts-15.10
-}

-- to run -> stack 6_exercises.hs

-- --------------------- Question 1 ---------------------
-- 1.​How does the recursive version of the factorial function behave if applied
--  to a negative argument, such as (-1)?
--  Modify the definition to prohibit negative arguments by adding a guard
--  to the recursive case.

factorial' :: Integral a => a -> a
factorial' n
  | n < 0 = 1
  | n == 0 = 1
  | otherwise = n * factorial' (n - 1)

-- --------------------- Question 2 ---------------------
-- 2.​Define a recursive function sumdown :: Int -> Int that returns
-- the sum of the non-negative integers from a given value down to zero.
-- For example, sumdown 3 should return the result 3+2+1+0 = 6.

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- --------------------- Question 3 ---------------------
-- 3.​Define the exponentiation operator ^ for non-negative integers
-- using the same pattern of recursion as the multiplication operator *,
-- and show how the expression 2 ^ 3 is evaluated using your definition.

(^) :: Int -> Int -> Int
_ ^ 0 = 1
m ^ 1 = m
m ^ n = m * (m Main.^ (n -1))

-- --------------------- Question 4 ---------------------
-- 4.​Define a recursive function euclid :: Int -> Int -> Int
-- that implements Euclid’s algorithm for calculating the greatest common divisor
-- of two non-negative integers:
--  if the two numbers are equal, this number is the result;
--  otherwise, the smaller number is subtracted from the larger, and the same process is then repeated.
--  For example: >  euclid 6 27 is 3

-- Based on https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclid's_algorithm
euclid :: Int -> Int -> Int
euclid a b
  | a == b = a
  | a > b = euclid (a - b) b
  | otherwise = euclid a (b - a)

-- --------------------- Question 5 ---------------------
-- 5.​Using the recursive definitions given in this chapter,
--  show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- 1 + length [2, 3]
-- 1 + (2 + length [3])
-- 1 + (2 + (3 + length []))
-- 1 + (2 + (3 + 0))

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n -1) xs

-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4, 5]
-- [4, 5]

-- library function `init` that removes the last elment from a non-empty list
init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs

-- init [1,2,3]
-- 1: init [2,3]
-- 1: 2: init [3]
-- 1: 2: []
-- [1, 2]

-- --------------------- Question 6 ---------------------
-- 6.​Without looking at the definitions from the standard prelude,
--  define the following library functions on lists using recursion.
--     a.​Decide if all logical values in a list are True:
--              and :: [Bool] -> Bool
--     b.​Concatenate a list of lists:
--              concat :: [[a]] -> [a]
--     c.​Produce a list with n identical elements:
--              replicate :: Int -> a -> [a]
--     d.​Select the nth element of a list:
--              (!!) :: [a] -> Int -> a
--     e.​Decide if a value is an element of a list:
--              elem :: Eq a => a -> [a] -> Bool
--         Note: most of these functions are defined in the prelude using other
--         library functions rather than using explicit recursion,
--         and are generic functions rather than being specific to the type of lists.
--

and' :: [Bool] -> Bool
and' [] = False -- is this right? Question unclear about this
and' [x] = x
and' (x : xs)
  | not x = False
  | otherwise = and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n -1) x

-- TODO Fix Non-exhaustive patterns in function
-- (!!) :: [a] -> Int -> a
-- (x : xs) !! 0 = x
-- (x : xs) !! n = xs Main.!! (n -1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs

-- --------------------- Question 7 ---------------------
-- 7.​Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
-- that merges two sorted lists to give a single sorted list.
-- For example: > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
-- Note: your definition should not use other functions on sorted lists
-- such as insert or isort, but should be defined using explicit recursion.

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x > y = y : merge (x : xs) ys
  | x < y = x : merge xs (y : ys)
  | otherwise = x : y : merge xs ys

-- --------------------- Question 8 ---------------------
-- 8.​Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort,
-- in which the empty list and singleton lists are already sorted,
-- and any other list is sorted by merging together the two lists
-- that result from sorting the two halves of the list separately.
-- Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list
-- into two halves whose lengths differ by at most one.

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort first) (msort second)
  where
    (first, second) = halve xs

-- --------------------- Question 9 ---------------------
-- 9.​Using the five-step process, construct the library functions that:
--  a.​calculate the sum of a list of numbers;
--  b.​take a given number of elements from the start of a list;
--  c.​select the last element of a non-empty list.
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n -1) xs

last' :: [a] -> a
last' [x] = x
last' (x : xs) = last' xs

-- ---------------------  ---------------------

main :: IO ()
main = do
  print ("6_exercises.hs")
  print (factorial' 2)
  print (factorial' 0)
  print (5 Main.^ 3)
  print (5 Main.^ 1)
  print (5 Main.^ 0) -- 1
  print (0 Main.^ 5) -- 0
  print (5 Main.^ 0) -- 1
  print (euclid 6 27)
  print (and' [True, False])
  print (and' [True, True])
  print (and' [True])
  print (and' [False])
  print (and' [])
  print (concat' [[1 .. 4], [11 .. 14]])
  print (concat' [[1 .. 4]])
  print (replicate' 3 11)
  print (replicate' 0 True)
  --   print ([3, 4, 5, 6] Main.!! 2)
  --   print ([3, 4, 5, 6] Main.!! 6)
  print (merge [2, 5, 6] [1, 3, 4])
  print (halve [1 .. 19])
  print (msort [1, 7, 2, 4, 9, 0, 4, 8, 7, 3, 6, 3, 9])
  print (sum' [10 .. 15])
  print (take' 6 [100 ..])
  print (last' [10 .. 15])
  print (length' [1, 2, 3])
  print (drop' 3 [1, 2, 3, 4, 5])
  print (init' [1, 2, 3])
--length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.
