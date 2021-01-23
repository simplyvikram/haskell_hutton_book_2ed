{- stack script
 --resolver lts-15.10
-}

-- import Data.Char ()
-- import System.IO (hSetEcho, stdin)

--

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

-- representations of the board, the cells where the living organisms are present
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- sequence_ :: [IO a] -> IO ()
-- sequence_ performs a list of actions on a sequence,
-- discards their return values and returns no result
showcells :: Board -> IO ()
showcells b = sequence_ [writeat pos "O" | pos <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x, y -1),
      (x, y + 1),
      (x -1, y),
      (x + 1, y),
      (x -1, y -1),
      (x + 1, y -1),
      (x -1, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) = (((x -1) `mod` width) + 1, ((y -1) `mod` height) + 1)

-- Calculates number of live neighbors
liveneighbs :: Board -> Pos -> Int
liveneighbs board = length . filter (isAlive board) . neighbs

-- list of living positions in a board, ones that survive the next generation
-- of the game, one with 2 or three neighbors
survivors :: Board -> [Pos]
survivors board = [pos | pos <- board, elem (liveneighbs board pos) [2, 3]]

main :: IO ()
main = do
  print ("game of life")
