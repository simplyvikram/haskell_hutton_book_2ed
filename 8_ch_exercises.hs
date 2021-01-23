{- stack script
 --resolver lts-15.10
-}

-- ------------------------------ Question 1 ------------------------------
-- 1.​In a similar manner to the function add, define a recursive multiplication
-- function mult :: Nat -> Nat -> Nat for
-- the recursive type of natural numbers:
-- Hint: make use of add in your definition.

data Nat = Zeroo | Succ Nat

nat2int :: Nat -> Int
nat2int Zeroo = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zeroo
int2nat n = Succ (int2nat (n -1))

add :: Nat -> Nat -> Nat
add Zeroo n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zeroo _ = Zeroo
mult (Succ m) n = add (mult m n) n -- (m + 1) * (n) = (m * n) + m

-- ------------------------------ Question 2 ------------------------------
-- 2.​Although not included in appendix B, the standard prelude defines
-- data Ordering = LT | EQ | GT
-- together with a function
-- compare :: Ord a => a -> a -> Ordering
--  that decides if one value in an ordered type is less than (LT),
--   equal to (EQ), or greater than (GT) another value.
-- Using this function, redefine the function
-- occurs :: Ord a => a -> Tree a -> Bool
-- for search trees.
-- Why is this new definition more efficient than the original version?

data Tree a = Leaff a | Nodee (Tree a) a (Tree a)

sometree :: Tree Int
sometree =
  Nodee
    (Nodee (Leaff 1) 3 (Leaff 4))
    5
    (Nodee (Leaff 6) 7 (Leaff 9))

occursOriginal :: Eq a => a -> Tree a -> Bool
occursOriginal x (Leaff y) = x == y
occursOriginal x (Nodee t1 y t2) = x == y || occursOriginal x t1 || occursOriginal x t2

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaff y) = x == y
occurs x (Nodee t1 y t2) = case compare x y of
  LT -> occurs x t1
  GT -> occurs x t2
  EQ -> True

-- ------------------------------ Question 3 ------------------------------
-- 3.​Consider the following type of binary trees:
--   data Tree a = Leaf a | Node (Tree a) (Tree a)
-- Let us say that such a tree is balanced if the number of leaves
--  in the left and right subtree of every node differs by at most one,
-- with leaves themselves being trivially balanced.
-- Define a function balanced :: Tree a -> Bool that decides
-- if a binary tree is balanced or not.
-- Hint: first define a function that returns the number of leaves in a tree.

leaves :: Tree Int -> Int
leaves (Leaff _) = 1
leaves (Nodee t1 _ t2) = (leaves t1) + (leaves t2)

balanced :: Tree Int -> Bool
balanced (Leaff _) = True
balanced (Nodee t1 _ t2) =
  abs ((leaves t1) - (leaves t2)) <= 1
    && balanced t1
    && balanced t2

-- ------------------------------ Question 4 ------------------------------
-- 4.​Define a function
-- balance :: [a] -> Tree a
-- that converts a non-empty list into a balanced tree.
-- Hint: first define a function that splits a list into two halves
-- whose length differs by at most one.

halves :: [a] -> ([a], [a])
halves xs = (take l xs, drop l xs)
  where
    l = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaff x
balance (x : xs) = Nodee (balance leftlist) x (balance rightlist)
  where
    (leftlist, rightlist) = halves xs

-- ------------------------------ Question 5 ------------------------------
-- 5.​Given the type declaration
-- data Expr = Val Int | Add Expr Expr
-- define a higher-order function
-- folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- such that `folde f g` replaces each Val constructor in an expression
--  by the function f, and each Add constructor by the function g.

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- ------------------------------ Question 6 ------------------------------
-- 6.​Using folde, define a function
-- eval :: Expr -> Int
--  that evaluates an expression to an integer value,
--  and a function
-- size :: Expr -> Int
-- that calculates the number of values in an expression.

eval :: Expr -> Int
eval = folde (+ 0) (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

-- ------------------------------ Question 7 ------------------------------
-- 7.​Complete the following instance declarations:
-- instance Eq a => Eq (Maybe a) where
--    ...
-- instance Eq a => Eq [a] where
--     ...

-- instance Eq a => Eq (Maybe a)

--TODO do we need to add Prelude/Main before operators

-- (==) Nothing Nothing = True
-- (==) (Just x) (Just y) = x Prelude.== y
-- (==) _ _ = False

-- instance Eq a => Eq [a]
-- (==) [] [] = True
-- (==) (x : xs) (y : ys) = x Prelude.== y and xy Main.== xy
-- (==) _ _ = False

-- ------------------------------ Question 8 ------------------------------
-- 8.​Extend the tautology checker to support the use of logical disjunction (V)
--  and equivalence (⇔) in propositions.

--Extended code in 8_ch_workbook_tautology.hs

-- ------------------------------ Question 9 ------------------------------
-- 9.​Extend the abstract machine to support the use of multiplication.

main :: IO ()
main = do
  print ("Question 1")
  print (nat2int (add (int2nat 8) (int2nat 9)))
  print (nat2int (mult (int2nat 8) (int2nat 9)))
  print (nat2int (mult (int2nat 0) (int2nat 9)))
  print (nat2int (mult (int2nat 8) (int2nat 0)))
  print ("Question 2")
  print (occursOriginal 7 sometree)
  print (occursOriginal 20 sometree)
  print (occurs 7 sometree)
  print (occurs 20 sometree)
  print (balanced sometree)
