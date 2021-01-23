{- stack script
 --resolver lts-15.10
-}

-- To run - stack 5_exercises.hs

-- --------------------- Question 1 ---------------------
-- Using a list comprehension, give an expression that calculates the sum
-- 1^2 + 2^2 + ... 100^2 of the first one hundred integer squares.
sumSquares :: Int
sumSquares = sum [x ^ 2 | x <- [1 .. 100]]

-- --------------------- Question 2 ---------------------
-- Suppose that a coordinate grid of size m × n is given by the list of
-- all pairs (x, y) of integers such that  0 <= x <=m and 0 <= y <= n
-- Using a list comprehension, define a function
-- grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a given size.
-- For example: > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- --------------------- Question 3 ---------------------
-- Using a list comprehension and the function grid above,
-- define a function square :: Int -> [(Int,Int)]
-- that returns a coordinate square of size n,
-- excluding the diagonal from (0, 0) to (n, n).
-- For example: > square 2 [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- --------------------- Question 4 ---------------------
-- In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements
-- can be defined using a list comprehension. For example: >
-- replicate 3 True
-- [True,True,True]

replicate :: Int -> a -> [a]
replicate n val = [val | x <- [1 .. n]]

-- --------------------- Question 5 ---------------------
-- A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation
-- x^2 + y^2 = z^2. Using a list comprehension with three generators,
-- define a function pyths :: Int -> [(Int,Int,Int)] that returns
-- the list of all such triples whose components are at most a given limit. For example: >
-- pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

-- --------------------- Question 6 ---------------------
-- A positive integer is perfect if it equals the sum of all of its factors,
-- excluding the number itself.
-- Using a list comprehension and the function factors,
-- define a function perfects :: Int -> [Int] that returns the list of all perfect numbers
-- up to a given limit. For example: >
-- perfects 500
-- [6,28,496]

factors :: Int -> [Int]
factors num = [x | x <- [1 .. num], num `mod` x == 0]

perfects :: Int -> [Int]
perfects num = [x | x <- [2 .. num], ((sum (factors x)) - x) == x]

-- --------------------- Question 7 ---------------------
-- Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]]
-- with two generators can be re-expressed using two comprehensions with single generators.
-- Hint: nest one comprehension within the other and
--  make use of the library function concat :: [[a]] -> [a].

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

nestedComprehension :: [a] -> [b] -> [(a, b)]
nestedComprehension xs ys = Main.concat [[(x, y) | y <- ys] | x <- xs]

-- --------------------- Question 8 ---------------------
-- Redefine the function `positions` using the function `find`.

-- List of all positions at which a value occurs in a list
positionsOriginal :: Eq a => a -> [a] -> [Int]
positionsOriginal val xs = [i | (x, i) <- zip xs [0 ..], x == val]

findOriginal :: Eq a => a -> [(a, b)] -> [b]
findOriginal key pairs = [v | (k, v) <- pairs, key == k]

positionsNew :: Eq a => a -> [a] -> [Int]
positionsNew key xs = findOriginal key (zip xs [0 ..])

-- --------------------- Question 9 ---------------------
-- In a similar manner to chisqr, show how a list comprehension can be used
-- to define a function scalarproduct :: [Int] -> [Int] -> Int
-- that returns the scalar product of two lists. For example: >
-- scalarproduct [1,2,3] [4,5,6]
-- 32

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- --------------------- Question 10 ---------------------

main :: IO ()
main = do
  let l = [10 .. 20]
  print sumSquares
  print (grid 1 2)
  print (square 2)
  print (Main.replicate 3 True)
  print (pyths 10)
  print (factors 10)
  print (factors 34)
  print (perfects 500)
  print (Main.concat [[10, 20], [1, 2, 3], [99, 100]])
  print (nestedComprehension [1, 2] [3, 4])
  print (positionsNew 4 [4, 6, 7, 4, 1, 3, 5, 8, 4, 8, 4])
  print (scalarproduct [1, 2, 3] [4, 5, 6])
