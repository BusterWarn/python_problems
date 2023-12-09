module Day_09.Solution (
  solveFirst,
  solveSecond,
) where

import PuzzleReader (readInput)

parseInput :: FilePath -> IO [[Int]]
parseInput puzzleInputFile = do
  input <- readInput puzzleInputFile
  return $ map (map read . words) input

-- >>> sequentialDeltas [10,13,16,21,30,45]
-- [3,3,5,9,15]
sequentialDeltas :: [Int] -> [Int]
sequentialDeltas [] = error "Empty list"
sequentialDeltas oasisData = zipWith (-) (tail oasisData) oasisData

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  oasisData <- parseInput puzzleInputFile
  return $ sum $ map predictAndExtrapolate oasisData

-- >>> predictAndExtrapolate  [10,13,16,21,30,45]
-- 68
predictAndExtrapolate :: [Int] -> Int
predictAndExtrapolate xs = extrapolateHelper xs 0
 where
  extrapolateHelper ys acc
    | all (== 0) ys = acc
    | otherwise =
        let deltas = sequentialDeltas ys
            newAcc = acc + last ys
         in extrapolateHelper deltas newAcc

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  oasisData <- parseInput puzzleInputFile
  return $ sum $ map predictAndExtrapolateBackwards oasisData

-- Not tail call optimized
-- >>> predictAndExtrapolateBackwards  [10,13,16,21,30,45]
-- 5
predictAndExtrapolateBackwards :: [Int] -> Int
predictAndExtrapolateBackwards oasisData
  | all (== 0) oasisData = 0
  | otherwise =
      let deltas = sequentialDeltas oasisData
       in head oasisData - predictAndExtrapolateBackwards deltas
