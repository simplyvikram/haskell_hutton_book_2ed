{- stack script
 --resolver lts-15.10
-}

{--
COUNTDOWN PROBLEM.
The essence of the problem is as follows:
Given a sequence of numbers and a target number, attempt to construct an expression
 whose value is the target, by combining one or more numbers from the sequence
  using addition, subtraction, multiplication, division and parentheses.
Each number in the sequence can only be used at most once in the expression,
 and all of the numbers involved, including intermediate values,
 must be positive natural numbers (1, 2, 3, . . .).
 In particular, the use of negative numbers, zero, and proper fractions such as 2 ÷ 3,
  is not permitted.
  For example, suppose that we are given the sequence 1, 3, 7, 10, 25, 50,
   and the target 765.
    Then one possible solution is given by the expression
    (1+50)∗(25–10)

In fact, for this example it can be shown that there are 780 different solutions.


--}

-- Eq is class type
-- a to be an instance of class Eq it must support eq and ineq operators
-- class Eq a where
--     (==), (/=) :: a -> a -> Bool
--     x /= y = not (x == y)

-- type Bool made into an equality tope
-- instance Eq Bool where
--     False == False = True
--     True == True = True
--     _ == _ = False

data Op = Addd | Subb | Multt | Divv

instance Show Op where
  show Addd = "+"
  show Subb = "-"
  show Multt = "*"
  show Divv = "/"

-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

-- with some optimzation for cases which get repeated
valid :: Op -> Int -> Int -> Bool
valid Addd x y = x <= y -- as the 2 + 3 case is covered by 3 + 2
valid Subb x y = x > y
valid Multt x y = x /= 1 && y /= 1 && x <= y
valid Divv x y = (x `mod` y) == 0 && y /= 1 -- as 2/1 and 2 are the same

apply :: Op -> Int -> Int -> Int
apply Addd x y = x + y
apply Subb x y = x - y
apply Multt x y = x * y
apply Divv x y = x `div` y

data Expr = Val Int | App Op Expr Expr

-- > show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-- > "1+(2*3)”
instance Show Expr where
  show (Val n) = show n
  show (App op e1 e2) = (f e1) ++ (show op) ++ (f e2)
    where
      f (Val x) = show x
      f e = "(" ++ (show e) ++ ")"

-- list of values in an expression
values :: Expr -> [Int]
values (Val x) = [x]
values (App _ e1 e2) = (values e1) ++ (values e2)

-- Note that the possibility of failure within `eval` is handled
-- by returning a list of results, with the convention that
-- a singleton list denotes success, and the empty list denotes failure.
eval :: Expr -> [Int]
eval (Val x) = [x]
eval (App op e1 e2) =
  [apply op x y | x <- (eval e1), y <- (eval e2), valid op x y]

expr1 :: Expr
expr1 = App Addd (Val 1) (App Multt (Val 2) (Val 3))

-------- Combinatorial functions

-- The function `subs` returns all subsequences of a list
-- which are given by all possible combinations of
-- excluding or including each element of the list,
-- subs ["a", "b"] -> [[],["b"],["a"],["a","b"]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = ys ++ map (x :) ys
  where
    ys = subs xs

-- `interleave` returns all possible ways of inserting a new element into a list,
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- `perms` returns all permutations of a list,
-- which are given by all possible reorderings of the elements:
-- perms ["a", "b"] -> [["a","b"],["b","a"]]
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

-- > choices [1,2,3]
-- > [
-- >        [],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],
-- >        [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]
--  ]
choices :: [a] -> [[a]]
choices = concat . map perms . subs

--   using list comprehension

choices' :: [a] -> [[a]]
choices' xs = [xsss | xss <- subs xs, xsss <- perms xss]

-- An expression is a solution for a given list of numbers
-- and a target if the list of values in the expression
-- is chosen from the list of numbers,
-- and the expression successfully evaluates to give the target.
solution :: Expr -> [Int] -> Int -> Bool
solution expr nums target =
  elem (values expr) (choices nums) && (eval (expr) == [target])

-- a function `split` that returns all possible ways of splitting a list into two
-- non-empty lists that append to give the original list:

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : l, r) | (l, r) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Addd, Subb, Multt, Divv]

-- `exprs`, which returns all possible expressions whose list of values
-- is precisely a given list:
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e
    | (ls, rs) <- split ns,
      l_exprs <- exprs ls,
      r_exprs <- exprs rs,
      e <- combine l_exprs r_exprs
  ]

-- function `solutions` that returns all possible expressions
-- that solve an instance of the countdown problem,
-- all expressions over each choice from the given list of numbers,
-- and then selecting those expressions that successfully evaluate
-- to give the target:
solutions :: [Int] -> Int -> [Expr]
solutions ns target =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [target]]

-- ----- optimzation -----

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
    | (ls, rs) <- split ns,
      l_results <- results ls,
      r_results <- results rs,
      res <- combine' l_results r_results
  ]

combine' :: Result -> Result -> [Result]
combine' (l_expr, x) (r_expr, y) =
  [(App o l_expr r_expr, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

main :: IO ()
main = do
  print ("test")
  print (show expr1)
  print (show (eval expr1))
  print (subs [1, 2, 3])
  print (interleave 1 [5 .. 8])
  print (perms ['a', 'b', 'c'])
  print (split [1, 2, 3, 4])
  print (choices [1, 2, 3])
  print (choices' [10, 20, 30])
--   print (solutions [1, 3, 7, 10, 25, 50] 765)
