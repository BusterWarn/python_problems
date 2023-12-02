module Main (main) where

import qualified Day_00.Solution as Day_00
import qualified Day_01.Solution as Day_01
import qualified Day_02.Solution as Day_02

main :: IO ()
main = do
  putStrLn "Enter the day number to run (e.g., 1 for Day01):"
  day <- getLine
  let puzzle_solver = case day of
        "0" -> Day_00.solve "src/Day_00/input.txt"
        "1" -> Day_01.solve_2 "src/Day_01/input.txt"
        "2" -> Day_02.solveSecond "src/Day_02/input.txt"
        _ -> return ["Day not recognized."]
  solution <- puzzle_solver -- Execute the puzzle solver
  mapM_ putStrLn solution -- Print each line of the solution
