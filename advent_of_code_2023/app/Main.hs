module Main (main) where

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
        "5" -> fmap show $ Day_05.solveFirst "src/Day_05/input.txt"
        "6" -> fmap show $ Day_06.solveSecond "src/Day_06/input.txt"
        "7" -> fmap show $ Day_07.solveFirst "src/Day_07/input.txt"
        "8" -> fmap show $ Day_08.solveFirst "src/Day_08/input.txt"
        "9" -> fmap show $ Day_09.solveSecond "src/Day_09/input.txt"
        _ -> return "Day not recognized."
      putStrLn solution
