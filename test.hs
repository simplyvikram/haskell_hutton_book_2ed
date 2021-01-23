double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Int -> Int
factorial n = product [1 .. n]

average' :: Foldable t => t Int -> Int
average' ns = sum ns `div` length ns

a :: Integer
a = b + c
  where
    b = 10; c = 20

d :: Integer
d = a * 2

n :: Int
n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

zeroto :: Int -> [Int]
zeroto n = [0 .. n]

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> (Int -> Int)
-- add' x y = x + y
add' = \x -> (\y -> x + y)

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z

foo :: Int -> (Int -> (Int -> (Int -> Int)))
foo a b c d = a * b * c * d

bbb :: Int -> (Int -> (Int -> Int))
bbb a b c = a * b * c

blahblah :: Int -> [a] -> ([a], [a])
blahblah n xs = (take n xs, drop n xs)

abs' :: Int -> Int
-- abs' n = if n >= 0 then n else - n
abs' n
  | n > 0 = n
  | otherwise = - n

signum' :: Int -> Int
-- signum' n =
--   if n < 0
--     then -1
--     else if n == 0 then 0 else 1
signum' n
  | n > 0 = 1
  | n < 0 = -1
  | otherwise = 0

(&&&) :: Bool -> Bool -> Bool
-- True &&& b = b
-- False &&& _ = False
b &&& c
  | b == c = b
  | otherwise = False

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, a) = a

testA :: [Char] -> Bool
testA ('a' : _) = True
testA _ = False

head' :: [a] -> a
head' (x : _) = x

tail' :: [a] -> [a]
tail' (_ : xs) = xs
