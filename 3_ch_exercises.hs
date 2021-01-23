{- stack script
 --resolver lts-15.10
-}

-- -------------------- Question 1 --------------------
-- What are the types of the following values?

-- ['a','b','c']                      [Char]
-- ('a', 'b', 'c')                    (Char, Char, Char)
-- [(False,'0'),(True,'1')]          [(Bool, Char)]
-- ([False,True],['0', '1'])         ([Bool], [Char])
-- [tail, init, reverse]             [[a] -> [a]]

-- -------------------- Question 2 --------------------
-- Write down definitions that have the following types;
-- it does not matter what the definitions actually do as long as they are type correct.

-- bools :: [Bool]
bools :: [Bool]
bools = [True, False]

-- nums :: [[Int]]
nums :: [[Int]]
nums = [[1 .. 4], [4 .. 10], [3, 4]]

-- add :: Int -> Int -> Int -> Int
add :: Int -> Int -> Int -> Int
add a b c = a + b + c

-- copy :: a -> (a,a)
copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f x = f x

-- -------------------- Question 3 --------------------
-- What are the types of the following functions?
-- second xs = head (tail xs)           second :: [a] -> a
-- swap (x,y) = (y,x)                   swap :: (b, a) -> (a, b
-- pair x y = (x,y)                     pair :: a -> b -> (a, b)
-- double x = x*2                       double :: Num a => a -> a
-- palindrome xs = reverse xs == xs     palindrome :: Eq a => [a] -> Bool
-- twice f x = f (f x)                  twice :: (t -> t) -> t -> t

main = do
  print "Ex 3"
