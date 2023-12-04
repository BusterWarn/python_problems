module Main (main) where

import qualified Day_00.Solution as Day_00
import qualified Day_01.Solution as Day_01
import qualified Day_02.Solution as Day_02
import qualified Day_03.Solution as Day_03
import qualified Day_04.Solution as Day_04

main :: IO ()
main = do
  putStrLn "Enter the day number to run (e.g., 1 for Day01):"
  day <- getLine
  case day of
    "0" -> Day_00.solve "src/Day_00/test_input.txt"
    _ -> do
      solution <- case day of
        "1" -> fmap show $ Day_01.solve_2 "src/Day_01/input.txt"
        "2" -> fmap show $ Day_02.solveSecond "src/Day_02/input.txt"
        "3" -> fmap show $ Day_03.solveSecond "src/Day_03/input.txt"
        "4" -> fmap show $ Day_04.solveSecond "src/Day_04/input.txt"
        _ -> return "Day not recognized."
      putStrLn solution
