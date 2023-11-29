module Main (main) where

import qualified Day_00.Solution as Day_00

main :: IO ()
main = do
  putStrLn "Enter the day number to run (e.g., 1 for Day01):"
  day <- getLine
  let puzzle_solver = case day of
        "0" -> Day_00.solve "src/Day_00/input.txt"  -- Case for day 0
        -- ... add cases for other days
        _   -> return ["Day not recognized."]      -- Default case
  
  solution <- puzzle_solver  -- Execute the puzzle solver
  mapM_ putStrLn solution    -- Print each line of the solution

