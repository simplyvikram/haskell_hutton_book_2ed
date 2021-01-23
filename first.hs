{- stack script
 --resolver lts-15.10
-}

sumList :: Num a => [a] ->a
sumList [] = 0
sumList (n : ns) = n + sumList ns



productList :: Num a => [a] -> a
productList [] = 1
productList (x: xs) = x * productList xs



















qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                    smaller = [a| a <-xs, a < x]
                    larger = [b| b<-xs, b>x]

qsortReversed :: Ord a => [a] -> [a]
qsortReversed [] = []
qsortReversed (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                    smaller = [a| a <-xs, a <= x]
                    larger = [b| b<-xs, b>x]


-- seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act: acts) = do
    x <- act
    xs <- seqn acts
    return (x: xs)

main :: IO ()
main = do
  print (sumList [10, 20, 100])
  print (sumList [10.1, 20.9, 100.5])
  print (productList [10,20,30,40])
  print (qsort [2,2,3,1,1, 2])
  print (qsort [10,100, 40, 30, 80, 50])
  print (qsortReversed [10,100, 40, 30, 80, 50])


-- sumList :: [Int] -> Int
-- sumList [] = 0
-- sumList (x:xs) = x + sumList xs
