{- stack script
 --resolver lts-15.10
-}

-- To run - stack 4_exercises.hs

-- --------------------- Question 1 ---------------------
-- Question 1. ​Using library functions, define a function
-- halve :: [a] -> ([a],[a]) that splits an even-lengthed list
-- into two halves.
--  For example: > halve [1,2,3,4,5,6] ([1,2,3],[4,5,6])

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

-- --------------------- Question 2 ---------------------
-- Define a function third :: [a] -> a that returns the third element
-- in a list that contains at least this many elements using:
-- a.​head and tail;
-- b.​list indexing !!;
-- c.​pattern matching.

thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a
thirdB xs = xs !! 2

thirdC :: [a] -> a
thirdC (_ : _ : x : xs) = x

-- --------------------- Question 3 ---------------------
-- Consider a function safetail :: [a] -> [a] that behaves in the same way
-- as tail except that it maps the empty list to itself
-- rather than producing an error. Using tail and the function null :: [a] -> Bool
-- that decides if a list is empty or not, define safetail using:
-- a.​a conditional expression;
-- b.​guarded equations;
-- c.​pattern matching.

safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] -> [a]
safetailB xs
  | null xs = []
  | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs = tail xs

-- --------------------- Question 4 ---------------------
-- In a similar way to && in section 4.4,
-- show how the disjunction operator || can be defined in four different
-- ways using pattern matching.

-- First way
-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False

-- Second way
-- (||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _ = True

-- Third way
-- (||) :: Bool -> Bool -> Bool
-- True || _ = True
-- False || b = b

-- Fourth way
(||) :: Bool -> Bool -> Bool
a || b
  | a == b = a
  | otherwise = True

-- --------------------- Question 5 ---------------------
-- Without using any other library functions or operators, show how the meaning of
--   the following pattern matching definition for logical conjunction &&
--   can be formalised using conditional expressions:
--        True && True = True
--        _    && _    = False

-- Answer
-- (&&) :: Bool -> Bool -> Bool
-- (&&) a b =
--   if a
--     then (if b then True else False)
--     else False

-- --------------------- Question 6 ---------------------
-- Do the same for the following alternative definition,
-- and note the difference in the number of conditional expressions that are required:
--   True && b​ = b
--   False && _ = False

-- Answer
(&&) :: Bool -> Bool -> Bool
(&&) a b = if a then b else False

-- --------------------- Question 7 ---------------------
-- Show how the meaning of the following curried function definition
-- can be formalised in terms of lambda expressions:
-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z

-- Answer
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))

-- --------------------- Question 8 ---------------------
-- The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping
--  a digit, and proceeds as follows:
--     consider each digit as a separate number;
--     moving left, double every other number from the second last;
--     subtract 9 from each number that is now greater than 9;
--     add all the resulting numbers together;
--     if the total is divisible by 10, the card number is valid.
--
-- Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9
-- if the result is greater than 9. For example:
--     > luhnDouble 3
--      6
--     > luhnDouble 6
--      3
--
-- Using luhnDouble and the integer remainder function mod,
-- define a function luhn :: Int -> Int -> Int -> Int -> Bool
-- that decides if a four-digit bank card number is valid. For example:
--     > luhn 1 7 8 4
--      True
--     > luhn 4 7 8 3
--      False

luhnDouble :: Int -> Int
luhnDouble x
  | double > 9 = double - 9
  | otherwise = double
  where
    double = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((a' + b + c' + d) `mod` 10) == 0
  where
    c' = luhnDouble c
    a' = luhnDouble a

main :: IO ()
main = do
  let l = [10 .. 20]
  print "Question 1"
  print (halve l)
  print "Question 2"
  print (thirdA l)
  print (thirdB l)
  print (thirdC l)
  print "Question 3"
  print (safetailA l) --   print (safetailA [])
  print (safetailB l) --   print (safetailB [])
  print (safetailC l) --   print (safetailC [])
  print "Question 4"
  print (True Main.|| True)
  print (True Main.|| False)
  print (False Main.|| True)
  print (False Main.|| False)
  print "Question 5 and 6"
  print (True Main.&& True)
  print (True Main.&& False)
  print (False Main.&& True)
  print (False Main.&& False)
  print "Question 7"
  print (mult 3 4 10)
  print "Question 8"
  print (luhn 1 7 8 4)
  print (luhn 4 7 8 3)
