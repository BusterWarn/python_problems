module Day_07.Solution (
  solveFirst,
  solveSecond,
  determineHandStrength,
) where

import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import GHC.Float (Floating (cos))
import PuzzleReader (readInput)

parseInput :: FilePath -> IO [(String, Int)]
parseInput puzzleInputFile = do
  input <- readInput puzzleInputFile
  return $ map parseLine input
 where
  parseLine line =
    let (strPart, numPart) = break (== ' ') line
     in (filter (/= ' ') strPart, read $ filter (/= ' ') numPart)

determineHandStrength :: String -> Int
determineHandStrength hand = case counts of
  _ | length counts == 1 -> 7 -- Five of a Kind
  _ | 4 `elem` counts -> 6 -- Four of a Kind
  _ | length counts == 2 && elem 3 counts -> 5 -- Full House
  _ | 3 `elem` counts -> 4 -- Three of a Kind
  _ | length (filter (== 2) counts) == 2 -> 3 -- Two Pair
  _ | 2 `elem` counts -> 2 -- One Pair
  _ | all (== 1) counts -> 1 -- All Unique
  _ -> error $ "Impossible! " ++ hand
 where
  counts = map length $ group $ sort hand

-- >>> isLeftHandStronger "KK677" "KTJJT"
-- GT
isLeftHandStronger :: String -> String -> Ordering
isLeftHandStronger leftHand rightHand =
  let leftHandScore = determineHandStrength leftHand
      rightHandScore = determineHandStrength rightHand
   in case compare leftHandScore rightHandScore of
        EQ ->
          if isLeftHandBruteForceStrongerThanRightHand leftHand rightHand
            then GT
            else LT
        ordering -> ordering

-- >>> isLeftHandBruteForceStrongerThanRightHand "33TT9" "33TT8"
-- True
-- >>> isLeftHandBruteForceStrongerThanRightHand "33TT7" "33TT8"
-- False
isLeftHandBruteForceStrongerThanRightHand :: String -> String -> Bool
isLeftHandBruteForceStrongerThanRightHand (l : lrest) (r : rrest)
  | getCardScore l > getCardScore r = True
  | getCardScore l < getCardScore r = False
  | otherwise = isLeftHandBruteForceStrongerThanRightHand lrest rrest
isLeftHandBruteForceStrongerThanRightHand _ _ = error "Hands are equal"

getCardScore :: Char -> Int
getCardScore c
  | c == '2' = 0
  | c == '3' = 1
  | c == '4' = 2
  | c == '5' = 3
  | c == '6' = 4
  | c == '7' = 5
  | c == '8' = 6
  | c == '9' = 7
  | c == 'T' = 8
  | c == 'J' = 9
  | c == 'Q' = 10
  | c == 'K' = 11
  | c == 'A' = 12
  | otherwise = error $ "Invalid Card: " ++ [c]

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  input <- parseInput puzzleInputFile
  let sortedInput = sortBy (\(left, _) (right, _) -> isLeftHandStronger left right) input
      sortedBidsWithIndex = zip [1 ..] (map (\(_, bid) -> bid) sortedInput)
  -- print sortedInput
  -- print sortedBidsWithIndex
  return $ sum $ map (\(index, bid) -> index * bid) sortedBidsWithIndex

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  input <- readInput puzzleInputFile
  return 1
