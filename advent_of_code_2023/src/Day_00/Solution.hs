module Day_00.Solution (
  solve,
) where

import PuzzleReader (readInput)

solve :: FilePath -> IO ()
solve puzzleInputFile = do
  hello <- readInput puzzleInputFile
  putStrLn $ unlines hello
