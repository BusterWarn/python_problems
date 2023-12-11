module Main (main) where

import System.Directory (doesFileExist)

import qualified Day_00.Solution as Day_00
import qualified Day_01.Solution as Day_01
import qualified Day_02.Solution as Day_02
import qualified Day_03.Solution as Day_03
import qualified Day_04.Solution as Day_04
import qualified Day_05.Solution as Day_05
import qualified Day_06.Solution as Day_06
import qualified Day_07.Solution as Day_07
import qualified Day_08.Solution as Day_08
import qualified Day_09.Solution as Day_09
import qualified Day_10.Solution as Day_10
import qualified Day_11.Solution as Day_11

main :: IO ()
main = do
  putStrLn "Enter the day number to run (e.g., 1 for Day01):"
  day <- getLine

  let puzzleInputFile = getPuzzleInput $ read day
  fileExists <- doesFileExist puzzleInputFile

  if not fileExists
    then putStrLn $ "File does not exist: " ++ puzzleInputFile
    else case day of
      "0" -> Day_00.solve puzzleInputFile
      "1" -> solvePuzzle Day_01.solve_1 Day_01.solve_2 puzzleInputFile
      "2" -> solvePuzzle Day_02.solveFirst Day_02.solveSecond puzzleInputFile
      "3" -> solvePuzzle Day_03.solveFirst Day_03.solveSecond puzzleInputFile
      "4" -> solvePuzzle Day_04.solveFirst Day_04.solveSecond puzzleInputFile
      "5" -> solvePuzzle Day_05.solveFirst Day_05.solveSecond puzzleInputFile
      "6" -> solvePuzzle Day_06.solveFirst Day_06.solveSecond puzzleInputFile
      "7" -> solvePuzzle Day_07.solveFirst Day_07.solveSecond puzzleInputFile
      "8" -> solvePuzzle Day_08.solveFirst Day_08.solveSecond puzzleInputFile
      "9" -> solvePuzzle Day_09.solveFirst Day_09.solveSecond puzzleInputFile
      "10" -> solvePuzzle Day_10.solveFirst Day_10.solveSecond puzzleInputFile
      "11" -> solvePuzzle Day_11.solveFirst Day_11.solveSecond puzzleInputFile
      _ -> putStrLn "Day not applicable"

getPuzzleInput :: Int -> String
getPuzzleInput dayNumber
  | dayNumber > 25 = error "Christmas is not applicable above day 25"
  | dayNumber < 10 = "src/Day_0" ++ show dayNumber ++ "/input.txt"
  | otherwise = "src/Day_" ++ show dayNumber ++ "/input.txt"

solvePuzzle :: (FilePath -> IO Int) -> (FilePath -> IO Int) -> FilePath -> IO ()
solvePuzzle solveFirst solveSecond puzzleInputFile = do
  solutionFirst <- solveFirst puzzleInputFile
  solutionSecond <- solveSecond puzzleInputFile
  putStrLn $ "Solution to first: " ++ show solutionFirst
  putStrLn $ "Solution to second: " ++ show solutionSecond
