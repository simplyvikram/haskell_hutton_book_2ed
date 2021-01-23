{- stack script
 --resolver lts-15.10
-}

import Data.Char
import System.IO (hSetEcho, stdin)

-- type IO = World -> World
-- type IO a = World -> (a, World)     -- Called actions
-- IO Char -- type of action that returns Char
-- IO ()   -- type of action that returns empty tuple
-- type IO Char :: World -> (Char, World) -- return plus side effect
-- type IO ()   :: World -> ((), World)   -- returns empty tuple
--                                        -- Can be thought of as purely side effect

-- Interactive program that takes a character and returns an integer would have type
--              Char -> IO Int
-- which abbreviates to
--              Char -> World -> (Int World)
-- getChar :: IO Char
-- getChar c = ...
-- reads character from keyboard, echoes it to screen, and returns the result value

-- putChar :: Char -> IO ()
-- putChar c = ...
-- writes character c to screen, returns no result value,

-- return :: a -> IO a
-- return v = ...
-- returns the result value v without performing an interaction with user

act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine'
      return (x : xs)

--Writes Line to screen, no return value
-- String -> (World -> ((), World))
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
  putStr' xs
  putChar '\n'

strLen' :: IO ()
strLen' = do
  putStr' "Enter a string"
  putChar '\n'
  xs <- getLine'
  putStr' "The line has "
  putStr' (show (length xs))
  putStr' " characters.\n"

-- ------------- HANGMAN -------------

-- Reads a string of characters from the keyboard in a similar
-- manner to the basic action getLine, except that it echoes each
-- character as a dash symbol
sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs)

-- Reads a character from keyboard, does NOT echo to screen and returns it
-- Uses hSetEcho primitive from System.IO to turn input echoing off
-- prior to reading the character
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

hangman :: IO ()
hangman = do
  putStrLn "Think of a word"
  word <- sgetLine
  putStrLn "Try to guess it"
  play word

play :: String -> IO ()
play word = do
  putStr' "?\n"
  guess <- getLine'
  if guess == word
    then putStrLn' "You got it!!"
    else do
      putStrLn' (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '_' | x <- xs]

-- -------------- NIM GAME --------------

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row move = (board !! (row - 1)) >= move

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update rr nn = if rr == row then nn - num else nn

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

-- Could also have done the below
-- putRow row num = do
--   putStr x
--   where
--     x = (show row) ++ " : " ++ (concat (replicate num "* "))

-- getDigit that displays a prompt and reads a single character from the
-- keyboard. If character is a digit, the corresponding integer is returned,
-- otherwise an error message is displayed, and the user is re-prompted
getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  getChar
  newLine
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

newLine :: IO ()
newLine = putChar '\n'

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

playNim :: Board -> Int -> IO ()
playNim board player = do
  newLine
  putBoard board
  if (finished board)
    then do
      newLine
      putStr ("Player " ++ (show (next player)) ++ " wins!\n")
    else do
      newLine
      putStr ("Player " ++ (show player) ++ "\n")
      row <- getDigit "Enter a row number "
      num <- getDigit "Stars to remove "
      if valid board row num
        then playNim (move board row num) (next player)
        else do
          newLine
          putStrLn "ERROR: Invalid move"
          playNim board player

main :: IO ()
main = do
  print ("test")
  -- prompts a string to be read from the keyboard
  -- hangman
  -- putBoard initial
  playNim initial 1
