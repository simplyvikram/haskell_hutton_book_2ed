{- stack script
 --resolver lts-15.10
-}

import Data.List (sort)

-- ----------------------- Voting system ------------------------

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count elem = length . (filter (== elem))

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : filter (/= x) (removeDuplicates xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- removeDuplicates vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- ----------------------- Alternative vote ------------------------

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

-- remove empty ballots
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- remove a candidate from a ballot
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- TODO commenting for now, can't resolve weird lexical error
-- winner' :: Ord a => [[a]] -> a
-- winner' bs = case (rank (rmempty bs)) of
--             [c]​ -> x
--             (c:cs) -> winner' (elim c bs)

main :: IO ()
main = do
  print (ballots)
  print (elim "Green" ballots)
  print (rmempty ([] : [] : ballots))
  print ("-- Voting algorithm")
  print (votes)
  print (count "Blue" votes)
  print (removeDuplicates votes)
  print (winner votes)
--   print (winner' ballots)
