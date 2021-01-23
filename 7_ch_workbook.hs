{- stack script
 --resolver lts-15.10
-}

-- to run -> stack 7_play.hs

import Data.Char (chr, ord)

addCurried :: Int -> (Int -> Int)
addCurried = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- map first definition
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

-- map recursive defintion
mapRecursive :: (a -> b) -> [a] -> [b]
mapRecursive f [] = []
mapRecursive f (x : xs) = (f x) : map' f xs

-- map can be applied to itself to process nested lists
-- map (map (+ 1)) [[1, 2], [10, 20], [5, 6, 7]]
-- [map (+ 1) [1, 2], map (+ 1) [10, 20], map (+ 1) [5, 6, 7]]
-- [[2, 3], [11, 21], [6, 7, 8]]

-- filter first definition
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

filterRecursive :: (a -> Bool) -> [a] -> [a]
filterRecursive f [] = []
filterRecursive f (x : xs)
  | f x = x : filterRecursive f xs
  | otherwise = filterRecursive f xs

-- Sum of squares of even integers
sumsqreven :: [Int] -> Int
sumsqreven xs = sum (map (^ 2) (filter even xs))

-- Other higher-order functions for list processing `all` `any` `takeWhile` `dropWhile`
-- Select elements from a list while they satisfy a predicate:
--   > takeWhile even [2,4,6,7,8]
--   > [2,4,6]
--  Remove elements from a list while they satisfy a predicate:
--   > dropWhile odd [1,3,5,6,7]
--   > [6,7]

-- ----------------------- FOLDR  -----------------------
{--
Recursive pattern
f [] = v
f (x:xs) x # f xs

can be captured by `foldr # v`
so instead of defining f and calling `f some_list`, do `folder (#) v some_list`

(foldr (+) 0 ) [1,2,3]
1 + ((foldr (+) 0 ) [2,3])
1 + (2 + ((fold (+) 0) [3]))
1 + (2 + (3 +  ((fold (+) 0) [])   ))
1 + (2 + (3 + 0))


foldr - reflects use of an operator that is associated to the right
fold (#) v [x0, x1, .....xn] = x0 # (x1 # (x2 # ..(..(xn # (v)))))

e.g sum defintion using recursion
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

sumUsingFoldr = foldr' (+) 0

Think of it as replacing the `cons` operator by `f` and empty list by `v` so
`sum` using `foldr` would
convert 1 : (2 : (3 : [])) to 1 + (2 + (3 + v))

--}

-- f is a function
-- v is of type b
-- f is function where 1st arg is a, 2nd arg is of type b and return is of type b
-- so type of f is (a -> b -> b)
-- so type of foldr` is (a->b -> b) -> b -> [a] -> [b]
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

sum' :: Num a => [a] -> a
sum' = foldr' (+) 0

product' :: Num a => [a] -> a
product' = foldr' (*) 1

-- Or recursively
-- > or []​= False
-- > or (x:xs) = x || or xs

or' :: [Bool] -> Bool
or' = foldr' (||) False

-- And recursively
-- and []​= True
-- and (x:xs) = x && and xs
and' :: [Bool] -> Bool
and' = foldr' (&&) True

-- length recursively
-- length' :: [a] -> Int
-- length' [] = 0
-- length' (x:xs) = 1 + length' xs

-- length' using foldr'
-- why the lambda? (https://stackoverflow.com/a/11425454)
--         _ acts as element of [a] and n is the second arg, used as accumulator
--         n represents whatever is received from the recursive call of the sublist
length' :: [a] -> Int
length' = foldr' (\_ n -> 1 + n) 0

----------------------- FOLDL  -----------------------

{--
Recursive pattern for foldr
    f [] = v
    f (x:xs) x # f xs
can be captured by `foldr # v`

~ foldr a + (b + (c + d)) ~ associate to the right


Recursive pattern for foldl
    f v [] = v
    f v (x:xs) =  f (v # x) xs
can be captured by `foldl # v`

~ foldl (((a) + b) + c) + d ~ associate to the left


--}

-- foldl'
-- if output is of type a
-- xs wold be of type [b]
-- this means v is of type a
-- this means f has to take in inpts of a and b and convert to a

sumLeftAssociate :: Num a => [a] -> a
sumLeftAssociate = sumLeft 0
  where
    sumLeft v [] = v
    sumLeft v (x : xs) = sumLeft (v + x) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

sum'' :: Num a => [a] -> a
sum'' = foldl' (+) 0

product'' :: Num a => [a] -> a
product'' = foldl' (*) 1

or'' :: [Bool] -> Bool
or'' = foldl' (||) False

and'' :: [Bool] -> Bool
and'' = foldl' (&&) True

length'' :: [a] -> Int
length'' = foldl' (\v _ -> 1 + v) 0

-- Accumulator carries the reversed list uptil now,  while traversing left to right
reverse'' :: [a] -> [a]
reverse'' = foldl' (\xs x -> x : xs) []

-- foldl - reflects use an operator that is associated to the left
-- fold # v [x0, x1, x2 ..... xn] = ((((v # x0) # x1) # x2)..... # xn)

-- Contract this with foldr below

-- foldr - reflects use of an operator that is associated to the right
-- fold (#) v [x0, x1, .....xn] = x0 # (x1 # (x2 # ..(..(xn # (v)))))

--- ----------------------- COMPOSITION ------------------------

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)

-- odd' n = not (even n)
-- twice' f x = f (f x)
-- sumsqreven' = sum map (2^) (filter even xs)

odd' :: Integer -> Bool
odd' = not . even

twice' :: (b -> b) -> b -> b
twice' f = f . f

sumsqreven' :: [Int] -> Int
sumsqreven' = sum . map (^ 2) . filter even

--- ----------------------- BINARY STRING TRANSMITTER ------------------------

-- 1011 = (1 * 1) +(2 * 0) +(4 * 1) +(8 * 1)

type Bit = Int

-- Trying to convert binary number to decimal
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

-- Note - iterate produces an infinite list by applying a function an increasing number
-- of times to a value:
-- iterate f x = [x, f x, f (f x), f (f (fx)), ... ]

-- eg (1 * a) +(2 * b) +(4 * c) +(8 * d)
-- a + 2* (b + 2c + 4d)
-- a + 2(b + 2(c + 2d))
-- a + 2(b + 2(c + 2(d + 2(0))))
-- bin2int can be rewritten using foldr

-- foldr' f acc [] = acc
-- foldr' f acc (x : xs) = f x (foldr' f acc xs)
-- in2int'
bin2int' :: [Bit] -> Int
bin2int' = foldr' (\x acc -> x + 2 * acc) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- padd with extra zeros
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ (repeat 0))

-- We can encode using composition because the types match

-- * Main> :t  ord
-- ord :: Char -> Int

-- * Main> :t int2bin
-- int2bin :: Int -> [Bit]

-- * Main> :t make8
-- make8 :: [Bit] -> [Bit]

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = (map (chr . bin2int)) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

main :: IO ()
main = do
  print (twice (* 3) 4)
  print (twice reverse [1, 2, 3, 4])
  let r = twice (drop 2) [1 .. 10]
  print (r)
  print (twice (drop 2) r)
  print ("7_play.hs")
  print (addCurried 10 5)
  print (map' (* 10) [10, 20, 30])
  print (mapRecursive (* 10) [10, 20, 30])
  print (filter' (>= 4) [1 .. 7])
  print (filterRecursive (>= 4) [1 .. 7])
  print ("---Sumsqreven")
  print (sumsqreven [1 .. 10])
  print (sumsqreven' [1 .. 10])
  print (sum' [10 .. 13])
  print (sum'' [10 .. 13])
  print (sumLeftAssociate [10 .. 13])
  print (product' [10 .. 13])
  print (product'' [10 .. 13])
  print (length' [10 .. 16])
  print (length'' [10 .. 16])
  print (reverse'' [10 .. 16])
  print (odd' 13)
  print ("--bin2int--")
  print (bin2int [1, 0, 1, 1])
  print (bin2int' [1, 0, 1, 1])
  print ("int2bin")
  print (int2bin 13)
  print (decode (encode "abc"))
  print (transmit "transmitting a random string")
