{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day_08.Solution (
  solveFirst,
  solveSecond,
) where

import Data.Char (isAlpha)
import PuzzleReader (readInput)

type Path = (String, String, String)

parseInput :: FilePath -> IO (String, [Path])
parseInput puzzleInputFile = do
  input <- readInput puzzleInputFile
  let first = head input
      second = map (parseLine . filter isAlpha) $ drop 2 input
  return (first, second)
 where
  parseLine line
    | length line /= 9 = error $ "Invalid line " ++ line
    | otherwise =
        let
          firstThree = take 3 line
          middleThree = take 3 $ drop ((length line `div` 2) - 1) line
          lastThree = reverse $ take 3 $ reverse line
         in
          (firstThree, middleThree, lastThree)

getInstructionByIndex :: Int -> [Char] -> Char
getInstructionByIndex index instructions = instructions !! (index `mod` length instructions)

getNextStep :: Path -> Char -> String
getNextStep (_, left, right) c
  | c == 'L' = left
  | c == 'R' = right
  | otherwise = error $ "Invalid path: " ++ [c]

-- Key Value Map but it's a list what?
getValueFromKey :: String -> [Path] -> Path
getValueFromKey key [] = error $ "Ran out of values. Key: " ++ key
getValueFromKey myKey ((otherKey, leftValue, rightValue) : rest)
  | myKey == otherKey = (otherKey, leftValue, rightValue)
  | otherwise = getValueFromKey myKey rest

-- Part 1

parseThroughMap :: [Char] -> [Path] -> Int
parseThroughMap instructions network = go "AAA" 0 1
 where
  go _ _ 999999 = error "Steps are too darn high: 999999."
  go key instructionsIndex steps =
    let nextInstruction = getInstructionByIndex instructionsIndex instructions
        currentPath = getValueFromKey key network
        nextLocation = getNextStep currentPath nextInstruction
     in case nextLocation of
          "ZZZ" -> steps
          _ -> go nextLocation (instructionsIndex + 1) (steps + 1)

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  (instructions, network) <- parseInput puzzleInputFile
  return $ parseThroughMap instructions network

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  input <- readInput puzzleInputFile
  return 1
