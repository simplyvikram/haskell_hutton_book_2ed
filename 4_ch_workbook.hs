-- CONDITIONAL EXPRESSIONS
signum' :: Int -> Int
-- signum' n =
--   if n < 0
--     then -1
--     else if n > 0 then 1 else 0

-- GUARDED EXPRESSIONS
signum' n
  | n > 0 = n
  | n < 0 = -1
  | otherwise = 0

abs' :: (Num a, Ord a) => a -> a
abs' n
  | n >= 0 = n
  | otherwise = - n

-- (&&) :: Bool -> Bool -> Bool
-- True && True = True
-- _ && _ = False

-- (&&) :: Bool -> Bool -> Bool
-- True && b = b
-- False && _ = False

-- (&&) :: Bool -> Bool -> Bool
-- b && b
--   | b == c = b
--   | otherwise = False

-- * Main> True Main.&& True

-- LIST PATTERNS

-- Check if a list of characters begins with a 'a'
testBeginsWithA :: [Char] -> Bool
testBeginsWithA ('a' : _) = True
testBeginsWithA _ = False

-- LAMBDA EXPRESSIONS
-- Main> (\x -> x + x) 2
-- 4

addWithLambda :: Int -> (Int -> Int)
addWithLambda = \x -> (\y -> x + y)

-- Library function `const` that returns a constant function
-- that always produces a given value
const :: a -> b -> a
const x _ = x

-- Same as above but with using lambdas
constWithLambda :: a -> (b -> a)
constWithLambda x = \_ -> x

-- A function odds that returns the first n odd integers
odds :: Int -> [Int]
odds n = map f [0 .. (n -1)] where f x = 2 * x + 1

-- Same as above but with using lambda
oddsWithLambda :: Int -> [Int]
oddsWithLambda n = map (\x -> 2 * x + 1) [0 .. (n -1)]

--- OPERATOR SECTIONS
-- (#) = \x -> (\y -> x # y)
-- (x #) = \y -> x # y
-- (# y) = \x -> x # y
-- e.g
-- (1/)     \y -> 1 / y
-- (/2)     \x -> x / 2
-- (*2)     \x -> x * 2

-- * Main> :type (+)
-- (+) :: Num a => a -> a -> a

-- * Main>

-- * Main> :type (+4)
-- (+4) :: Num a => a -> a

-- * Main>

-- * Main> :type (4+)
-- (4+) :: Num a => a -> a
