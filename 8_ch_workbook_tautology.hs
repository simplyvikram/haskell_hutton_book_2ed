{- stack script
 --resolver lts-15.10
-}

{--

To see definition of imply
https://en.wikipedia.org/wiki/List_of_logic_symbols
language of Propositions built using basic values (True, False)
                                           variables(A, B, Z)
                                           negation, conjunction, implication
                                            and parantheses
Truth tables for negation, conjunction and implication definitions in hutton boook
Using them truth table of any proposition can be constructed.

The four propositions
    A ^ (Not A)
    (A ^ B) => A
    A => (A ^ B)
    (A ^ (A => B)) => B

If a proposition is True its considered a `tautology`
Determine if these propositions are tautologies
--}

-- Added OR and EQUIV as part of Q 8
data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 =
  Imply
    (And (Var 'A') (Imply (Var 'A') (Var 'B')))
    (Var 'B')

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find key t = head [v | (k, v) <- t, key == k]

type Subst = Assoc Char Bool -- for e.g [(’A’,False),(’B’,True)]

-- Remove duplicates
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- function that evaluates a proposition
-- logical implication operator is written backwards in Haskell!!
-- -- http://neilmitchell.blogspot.com/2007/02/logical-implication-in-haskell.html
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var v) = find v s
eval s (Not p1) = not (eval s p1)
eval s (And p1 p2) = (eval s p1) && (eval s p2)
eval s (Imply p1 p2) = (eval s p1) <= (eval s p2)
eval s (Or p1 p2) = (eval s p1) || (eval s p2)
eval s (Equiv p1 p2) = (eval s p1) == (eval s p2)

-- find all variables in a proposition
-- for eg `vars p2 = ['A', 'B', 'A']`
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var v) = [v]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p1 ++ vars p2
vars (Equiv p1 p2) = vars p1 ++ vars p2

-- Generate all possible combinations of booleans in a list of size n
-- for eg bools 3 will generate 8 lists of Bools of size 3 each
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) xs ++ map (True :) xs
  where
    xs = bools (n - 1)

-- generates all possible substitutions for a proposition
-- > substs p2
-- [
--     [(’A’,False),(’B’,False)],
--     [(’A’,False),(’B’,True)],
--     [(’A’,True),(’B’,False)],
--     [(’A’,True),(’B’,True)]
-- ]

all_substitutions :: Prop -> [Subst]
all_substitutions p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- all_substitutions p]

main :: IO ()
main = do
  print ("test")
  print (isTaut p1)
  print (isTaut p2)
  print (isTaut p3)
  print (isTaut p4)
