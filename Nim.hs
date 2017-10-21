module Main where 

import System.Random (randomRIO)
import Data.Function ((&))
import Data.List (transpose, intersperse)
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import Data.Bits (xor,testBit)
import Data.List (findIndices)
import System.IO (hFlush, stdout)

type Board = [Int]
data Player = Human | Computer deriving (Show,Eq)
type Move = (Int,Int)

maxCols = 6
maxRows = 8

instructions =
 ["Welcome to the game of Nim!",
 "Take it in turns to remove counters from the board - as many as you like from one column at a time.",
 "The aim of the game is to take the last counter.",
 "",
 "Enter your turn in the format \'A x\', where A is the letter labelling the column you want to remove from and x is the number of counters you want to remove from that column."]


-- main game functions

main :: IO ()
main =
 printInstructions >>
 getRandomBoard >>= \board ->
 displayBoard board >>
 getStartingPlayer >>= \startingPlayer ->
 gamePlay startingPlayer board

printInstructions :: IO ()
printInstructions = do
 putStrLn ""
 putStrLn $ unlines instructions

getRandomBoard :: IO Board
getRandomBoard = 
 randomRIO (2,maxCols) >>= \cols -> 
 sequence $ replicate cols $ randomRIO (1,maxRows)

displayBoard :: Board -> IO ()
displayBoard board = do
 putStrLn ""
 putStr $ unlines $ map (intersperse ' ') $
  [take (length board) (enumFrom 'A')] ++ 
   (transpose $ map (\x -> take (maximum board) (replicate x '*' ++ repeat ' ')) $ board)

getStartingPlayer :: IO Player
getStartingPlayer =
 putStrLn "" >>
 putStr "Would you like to go first or second (1/2)?: " >> hFlush stdout >>
 getLine >>= \response ->
 case response of
  "1" -> return Human
  "2" -> return Computer
  otherwise -> getStartingPlayer


gamePlay :: Player -> Board -> IO ()

gamePlay Human board = do
 putStrLn ""
 putStr "Your turn: "; hFlush stdout
 maybeMove <- (<$>) parseMove getLine
 case maybeMove of
  Nothing -> do
   putStrLn "Please enter your move in the format: letter number e.g. A 5"
   gamePlay Human board
  Just move -> do
   if (fst move < 0 || fst move >= length board || board !! fst move == 0)
    then gamePlay Human board
    else do
     let newBoard = boardSubtract move board
     displayBoard newBoard
     if all ((==) 0) newBoard 
      then putStrLn "\nYou Won!\n" 
      else gamePlay Computer newBoard

gamePlay Computer board = do
 putStrLn ""
 putStr "Computer's turn: "
 threadDelay 2000000
 move <- computerMove board
 putStrLn $ showMove move
 let newBoard = boardSubtract move board
 displayBoard newBoard
 if all ((==) 0) newBoard 
  then putStrLn "\nYou Lost!\n" 
  else gamePlay Human newBoard


-- supporting functions

parseMove :: String -> Maybe (Int, Int)
parseMove input = 
 case input of
  x:' ':xs -> (readMaybe xs :: Maybe Int) >>= \dy ->
              Just (fromEnum x - fromEnum 'A', dy)
  otherwise -> Nothing

showMove :: Move -> String
showMove (x, dy) = toEnum (x + fromEnum 'A'):' ':show dy

boardSubtract :: Move -> Board -> Board
boardSubtract move board = 
 zipWith (-) board (replicate (fst move) 0 ++ [snd move] ++ repeat 0) &
 map (\x -> if x < 0 then 0 else x)

computerMove :: Board -> IO Move
computerMove board = do
 let bitsum = foldr xor 0 board
 if bitsum == 0
  then do x <- randomPick $ findIndices (>0) board
          dy <- randomRIO (1, board !! x)
          return (x,dy)
  else do x <- randomPick $ findIndices (flip testBit (length (base 2 bitsum) - 1)) board
          let dy = y - xor bitsum y where y = board !! x
          return (x, dy)

randomPick :: [a] -> IO a
randomPick xs = (<$>) (xs!!) $ randomRIO (0, length xs - 1)

base :: Int -> Int -> [Int]
base b 0 = []
base b x = mod x b : base b (div x b)