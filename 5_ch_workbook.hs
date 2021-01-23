{- stack script
 --resolver lts-15.10
-}

import Data.Char

-- Concatenates a list of lists
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- Selects all the first components from a list of pairs
firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

evens :: Int -> [Int]
evens n = [x | x <- [1 .. n], even x]

-- List all factors for a number
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- Determine if a number is prime
prime :: Int -> Bool
prime n = factors n == [1, n]

-- Given a number, produce a list of all prime numbers upto that number
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find key pairs = [v | (k, v) <- pairs, key == k]

-- List of all positions at which a value occurs in a list
-- Use [0..] to avoid the need to explicityly produce a list of indices of the same
-- length as input
positions :: Eq a => a -> [a] -> [Int]
positions val xs = [i | (x, i) <- zip xs [0 ..], x == val]

-- number of lower case characters in a string
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- count of a particular character in a string
count :: Char -> [Char] -> Int
count ch xs = length [x | x <- xs, x == ch]

-- CAESER CIPHER
--lower case char to int between 0 and 25
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- performs the opposite of let2int
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- Caeser cipher encoding
encode :: Int -> String -> String
encode n xs = [shift n ch | ch <- xs]

-- Cracking the cipher
-- calcualte percentage of one integer wrt to another,
-- returning the floating point number
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- Using 'percent' 'lowers' and 'count' create function
-- that returns frequency table for a given string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) num_lower_chars | x <- ['a' .. 'z']]
  where
    num_lower_chars = lowers xs

-- calculate Chi square
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

-- rotate elements of list n places to the left
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs -- assume n is between 0 and size of list

-- By analysing a large volume of such text, one can derive the following table
-- of approximate percentage frequencies of the twenty-six letters of alphabet:
table :: [Float]
table =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

crack :: String -> String
crack xs = encode (factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

main :: IO ()
main = do
  print ("5_play.hs")
  print (Main.concat [[1, 2, 3], [10, 30], [9, 99, 99]])
  print (firsts [(1, 2), (10, 20), (100, 200)])
  print (length' [11 .. 20])
  print (evens 10)
  print (factors 44)
  print (prime 89)
  print ([(x, y) | x <- [1 .. 10], y <- [1 .. 10], x + y == 10])
  print (find 'b' [('a', 1), ('b', 2), ('c', 4), ('b', 5), ('a', 2)])
  print (positions False [True, False, True, False])
  print (lowers "TesTing")
  print (count 't' "testing tests")
  print (let2int 'b')
  print (int2let 1)
  print (encode 3 "haskell is fun") -- kdvnhoo lv ixq
  print (encode (-3) "kdvnhoo lv ixq") --  haskell is fun
  print (freqs "abbcccddddeeeee")
  print (chisqr [1.2, 1, 7, 1, 9] [3.3, 3.4, 3.5])
  print (crack "kdvnhoo lv ixq")
