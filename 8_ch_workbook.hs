{- stack script
 --resolver lts-15.10
-}

-- ----------- TYPE DECLARATIONS USING EXISTING TYPES
-- type String = [Char]
type Pos = (Int, Int)

type Trans = Pos -> Pos

-- can be parametrised by other types
type Pair a = (a, a)

--- types CANNOT be recursive
-- type Tree = (Int, [Tree]) -- Not allowed (error: Cycle in type synonym declarations)
-- each tree is an int and a list of subtrees
-- while its reasonable, with empty list of subtrees for base case of recursion,
-- its not permitted in Haskell

-- A type of lookup table that associates keys of one type to values of another type
-- can be decalred as a list of (key, val) pairs

-- e. g
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find key xs = head [v | (k, v) <- xs, k == key]

-- ------------ DATA DECLARATIONS

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y -1)
move East (x, y) = (x + 1, y)
move West (x, y) = (x -1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev East = West
rev South = North
rev West = East

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

circle :: Float -> Shape
circle = Circle

area :: Shape -> Float
area (Circle r) = pi * (r ^ 2)
area (Rect x y) = x * y

-- Standard prelde delcares Maybe
-- data Maybe a = Nothing | Just a
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

data Nat = Zero | Succ Nat

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n -1))

add :: Nat -> Nat -> Nat
add Zero n = n
add m n = int2Nat ((nat2Int m) + (nat2Int n))

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' m n)

-- Three different things, 1 & 2 are different types rather than synonyms
-- newtype Nat = N Int
-- type Nat = Int
-- data Nat = N Int

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

---------- Creating a Tree DataStructure
data Tree a = Leaff a | Nodee (Tree a) a (Tree a)

sometree :: Tree Int
sometree =
  Nodee
    (Nodee (Leaff 1) 3 (Leaff 4))
    5
    (Nodee (Leaff 6) 7 (Leaff 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaff y) = x == y
occurs x (Nodee t1 y t2) = x == y || occurs x t1 || occurs x t2

flatten :: Tree a -> [a]
flatten (Leaff x) = [x]
flatten (Nodee t1 x t2) = flatten t1 ++ [x] ++ flatten t2

-- If tree is ordered,
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaff y) = x == y
occurs' x (Nodee t1 y t2)
  | y Prelude.> x = occurs' x t1 -- if x is 2 and y is 3, go to left subtree
  | y Prelude.< x = occurs' x t2 -- if x is 2 and y is 1, go to right subtree
  | otherwise = True

-- Type for trees where data is only in leaves
data Tree' a = Leaff' a | Nodee' (Tree' a) (Tree' a)

--Type for trees where data only in nodes
data Tree'' a = Leaff'' | Nodee'' (Tree'' a) a (Tree'' a)

-- Tye for trees with different types of data in nodes and leaves
data Tree''' a b = Leaf''' a | Nodee''' (Tree''' a b) b (Tree''' a b)

-- Tree where each node has  data and a list of subtrees
data Tree'''' a = Node a [Tree'''' a]

----------------- CLASSES AND INSTANCE DECLARATIONS

-- class Eq' a where
--   (==), (/=) :: a -> a -> Bool
--   x /= y = not (x == y)

-- Making Bool into an instance of `Eq` type
-- Given above defintion, we only need defintion for == operator,
-- default definition is already included for /= operator

-- data Bool' = Truee | Falsee

-- instance Eq' Bool where
--   Falsee == Falsee = True
--   Truee == Truee = True
--   _ == _ = False

-- class Eq' a => Ord' a where
--   (>), (<), (<=), (>=) :: a -> a -> Bool
--   min', max' :: a -> a -> a
--   min' x y
--     | x Main.<= y = x -- Is this correct to do Main. ??
--     | otherwise = y
--   max' x y
--     | x Main.<= y = y
--     | otherwise = x

-- instance Ord' Bool' where
--   Falsee < Truee = True
--   _ < _ = False

--   x <= y = (x Main.< y) || (x == y)
--   x > y = y < x
--   x >= y = y <= x

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) (bools (n -1)) ++ map (True :) (bools (n -1))

main :: IO ()
main = do
  print ("alpha")
  print (move East (10, 10))
  print (area (square 7))
  print (area (circle 8))
  print (area (Circle 8))
  print ("---Testing out Maybe")
  print (safediv 10 0)
  print (safediv 10 4)
  print (safeHead [5 .. 8])
  print ("Nat2Int stuff")
  print (nat2Int (add (int2Nat 8) (int2Nat 10)))
  print (nat2Int (add' (int2Nat 8) (int2Nat 10)))
  let l = Cons 10 (Cons 20 (Cons 30 (Cons 4 Nil))) :: List Int
  print (len l)
  print ("Tree stuff")
  print (occurs 7 sometree)
  print (occurs 20 sometree)
  print (occurs' 7 sometree)
  print (occurs' 20 sometree)
  print (flatten sometree)
  print (bools 3)
--   let l = [2, 3, 4, 5]
--   print (len l)
--   print (show (safeHead [])) TODO figure out the issue
--                     Maybe/Show has with printing
