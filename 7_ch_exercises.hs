{- stack script
 --resolver lts-15.10
-}

{--

--}

-- ---------------------------- QUESTION 1 ----------------------------
-- 1.​Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
--  using the higher-order functions map and filter.

-- comprehension = map f (filter p xs)

-- ---------------------------- QUESTION 2 ----------------------------
-- 2.​Without looking at the definitions from the standard prelude, define the following
-- higher-order library functions on lists.
-- a.​Decide if all elements of a list satisfy a predicate:
--      all :: (a -> Bool) -> [a] -> Bool
-- b.​Decide if any element of a list satisfies a predicate:
--      any :: (a -> Bool) -> [a] -> Bool
-- c.​Select elements from a list while they satisfy a predicate:
--      takeWhile :: (a -> Bool) -> [a] -> [a]
-- d.​Remove elements from a list while they satisfy a predicate:
--      dropWhile :: (a -> Bool) -> [a] -> [a]
-- Note: in the prelude the first two of these functions are generic functions rather
--  than being specific to the type of lists.

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\x acc -> (f x) && acc) True

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> (f x) || acc) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x : xs)
  | f x = dropWhile' f xs
  | otherwise = x : xs

-- ---------------------------- QUESTION 3 ----------------------------
-- 3.​Redefine the functions map f and filter p using foldr.

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

-- ---------------------------- QUESTION 4 ----------------------------
-- 4.​Using foldl, define a function
-- dec2int :: [Int] -> Int that converts a decimal number into an integer. For example:
--     > dec2int [2,3,4,5]
--     > 2345

{--
A working example of the function argument of foldl
acc = 0, x = 2  -> ( acc * 10) + x -> 2
acc = 2, x = 3 -> (2 * 10) + 3 -> 23
acc = 23, x = 4 -> (23 * 10) + 4 -> 234
acc = 234, x = 5 -> (234 * 10) + 5 -> 2345
--}

-- Using foldl
dec2int' :: [Int] -> Int
dec2int' = foldl (\acc x -> (acc * 10) + x) 0

-- Using simple recursion
dec2int'' :: [Int] -> Int
dec2int'' = f 0
  where
    f v [] = v
    f v (x : xs) = f (v * 10 + x) xs -- # v x = (v * 10) + x

-- ---------------------------- QUESTION 5 ----------------------------
-- 5.​Without looking at the definitions from the standard prelude, define the
--  higher-order library function curry that converts a function on pairs
--  into a curried function, and, conversely, the function uncurry
--   that converts a curried function with two arguments into a function on pairs.
--    Hint: first write down the types of the two functions.

-- f takes a pair as a argument, but curryf takes two individual arguments
curryf :: ((a, b) -> c) -> (a -> b -> c)
curryf f = \x y -> f (x, y)

-- f takes two individual arguments, but uncurryf takes a pair
uncurryf :: (a -> b -> c) -> ((a, b) -> c)
uncurryf f = \(x, y) -> f x y

-- ---------------------------- QUESTION 6 ----------------------------
-- 6.​A higher-order function unfold that encapsulates a simple pattern of recursion
-- for producing a list can be defined as follows:
--            unfold p h t x | p x = []
--                           | otherwise = h x : unfold p h t (t x)
-- That is, the `function unfold p h t` produces the empty list
--          if the predicate p is true of the argument value,
-- and otherwise produces a non-empty list by applying
--      the function h to this value to give the head,
--      and the function t to generate another argument that is recursively processed
--      in the same way to produce the tail of the list.
-- For example, the function int2bin can be rewritten more compactly using unfold as follows:
--     int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
-- Redefine the functions chop8, map f and iterate f using unfold.

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- converts an integer to a array of bits
-- eg 5 becomes [1,0,1], 3 becomes [1, 1], 13 becomes [1,0,1,1]
int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' :: Int -> [Int]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- chops a list into sublists, each of length 8
-- original chop8
chop8 :: [Int] -> [[Int]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- chop8 using unfold
chop8' :: [Int] -> [[Int]]
chop8' = unfold null (take 8) (drop 8)

-- map using unfold
map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

-- Definition of iterate on hackage
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x == [x, f x, f (f x), ...]
-- iterate f x returns an infinite list of repeated applications of f to x:

-- iterate using unfold
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

-- ---------------------------- QUESTION 7 ----------------------------
-- 7.​Modify the binary string transmitter example to detect simple transmission errors
-- using the concept of parity bits.
-- That is, each eight-bit binary number produced during encoding is extended
-- with a parity bit, set to one if the number contains an odd number of ones,
--     and to zero otherwise.
-- In turn, each resulting nine-bit binary number consumed during decoding is checked
-- to ensure that its parity bit is correct, with the parity bit being discarded
--    if this is the case, and a parity error being reported otherwise.
--    Hint: the library function error :: String -> a displays
-- the given string as an error message and terminates the program;
-- the polymorphic result type ensures that error can be used in any context.
-- ---------------------------- QUESTION 8 ----------------------------
-- 8.​Test your new string transmitter program from the previous exercise using
-- a faulty communication channel that forgets the first bit,
-- which can be modelled using the tail function on lists of bits.

-- ---------------------------- QUESTION 9 ----------------------------
-- 9.​Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- that alternately applies its two argument functions to successive elements in a list,
-- in turn about order.
-- For example: > altMap (+10) (+100) [0,1,2,3,4]
--              > [10,101,12,103,14]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x : xs) = f x : altMap g f xs

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' f g = foo f g 0
  where
    foo f g _ [] = []
    foo f g v (x : xs)
      | v == 0 = (f x) : foo f g 1 xs
      | otherwise = (g x) : foo f g 0 xs

-- ---------------------------- QUESTION 10 ----------------------------
-- Using altMap, define a function luhn :: [Int] -> Bool
-- that implements the Luhn algorithm from the exercises in chapter 4 for
-- bank card numbers of any length.
-- Test your new function using your own bank card.

--  LUHN ALGORITHM FROM CHAPTER 4 BELOW
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

luhnWithAltMap :: [Int] -> Bool
luhnWithAltMap = (== 0) . (`mod` 10) . sum . map (`mod` 9) . altMap id (* 2) . reverse

main :: IO ()
main = do
  print ("hello")
  print (all' (> 10) [5 .. 15])
  print (all' (> 10) [20 .. 30])
  print (any' (> 10) [5 .. 15])
  print (any' (> 10) [20 .. 30])
  print (takeWhile' even [2, 4, 6, 7, 8]) -- [2,4,6]
  print (dropWhile' odd [1, 3, 5, 6, 7]) -- [6,7]
  print (map' (* 3) [10, 100, 1000])
  print (filter' (\x -> (x `mod` 5) == 0) [1 .. 100])
  print (dec2int' [2, 3, 4, 7])
  print (dec2int'' [2, 3, 4, 7])
  print ("Testing in2bin using unfold")
  print (int2bin 5)
  print (int2bin' 5)
  print (int2bin 13)
  print (int2bin' 13)
  print ("Testing chop and map using unfold")
  print (chop8 [10 .. 30])
  print (chop8' [10 .. 30])
  print (map (* 10) [15 .. 20])
  print (map'' (* 10) [15 .. 20])
  print (take 5 (iterate (* 2) 9))
  print (take 5 (iterate' (* 2) 9))
  print (altMap (+ 10) (+ 100) [0, 1, 2, 3, 4, 5, 6])
  print (altMap' (+ 10) (+ 100) [0, 1, 2, 3, 4, 5, 6])
  print ("Original luhn")
  print (luhn 1 7 8 4)
  print (luhn 4 7 8 3)
  print ("Altmap luhn")
  print (luhnWithAltMap [1, 7, 8, 4])
  print (luhnWithAltMap [4, 7, 8, 3])
