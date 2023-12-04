module Day_02.Solution (
  solveFirst,
  solveSecond,
) where

import Data.List.Split (splitOn)
import PuzzleReader (readInput, trimPrefixWithRegex)

-- Common code for both problems

data Color
  = Red Int
  | Green Int
  | Blue Int
  deriving (Show)

-- >>> parseLineIntoColors $ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- [[Blue 3,Red 4],[Red 1,Green 2,Blue 6],[Green 2]]
parseLineIntoColors :: String -> [[Color]]
parseLineIntoColors input_line = do
  let lineTrimmed = trimPrefixWithRegex input_line "Game [0-9]+: " -- "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
      gamesAsStrings = splitOn "; " lineTrimmed -- ["3 blue, 4 red"], ["1 red, 2 green, 6 blue"], ["2 green"]
      gamesAsSplitString = map (splitOn ", ") gamesAsStrings -- ["3 blue", "4 red"], ["1 red, "2 green, "6 blue"], ["2 green"]
  map (map parseColorFromString) gamesAsSplitString

-- >>> parseColorFromString $ "3 blue"
-- Blue 3
parseColorFromString :: String -> Color
parseColorFromString str =
  case parts of
    [value, "blue"] -> Blue $ read value
    [value, "red"] -> Red $ read value
    [value, "green"] -> Green $ read value
 where
  parts = splitOn " " str

countColors :: [Color] -> (Int, Int, Int)
countColors = foldl count (0, 0, 0)
 where
  count (r, g, b) (Red n) = (r + n, g, b)
  count (r, g, b) (Green n) = (r, g + n, b)
  count (r, g, b) (Blue n) = (r, g, b + n)

-- Part 1
-- Yeah I definetly overdid part 1, could have been really easy...

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  input <- readInput puzzleInputFile
  let linesOfColors = map parseLineIntoColors input
      linesPossible = zip [1 ..] (map isGamePossible linesOfColors)
      sumOfPossibleGameIndices = sum . map fst . filter snd $ linesPossible :: Int
  return $ sumOfPossibleGameIndices

isGamePossible :: [[Color]] -> Bool
isGamePossible [] = True
isGamePossible (cs : rest)
  | areColorsPossible cs = isGamePossible rest
  | otherwise = False

areColorsPossible :: [Color] -> Bool
areColorsPossible colors =
  let (r, g, b) = countColors colors
   in r <= 12 && g <= 13 && b <= 14

-- Well implemented but does not actually solve the correct problem...
-- getScoreFromListOfColors :: [[Color]] -> Maybe Int
-- getScoreFromListOfColors colors = go colors 0 0 0
--  where
--   go [] r g b
--     | r > 12 || g > 13 || b > 14 = Nothing
--     | otherwise = Just (r + g + b)
--   go (cs : rest) red green blue =
--     let (r, g, b) = foldl update (red, green, blue) cs
--      in go rest r g b
--
--   update (r, g, b) (Red n) = (r + n, g, b)
--   update (r, g, b) (Green n) = (r, g + n, b)
--   update (r, g, b) (Blue n) = (r, g, b + n)

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  input <- readInput puzzleInputFile
  let linesOfColors = map parseLineIntoColors input
      leastPossibleLines = map fewestNumberOfCubesOfEachColor linesOfColors
  return $ sum $ leastPossibleLines

fewestNumberOfCubesOfEachColor :: [[Color]] -> Int
fewestNumberOfCubesOfEachColor colors = go colors (0, 0, 0)
 where
  go [] (r, g, b) = r * g * b
  go (cs : rest) (r, g, b) = go rest $ foldl update (r, g, b) cs

  update (r, g, b) (Red n)
    | n > r = (n, g, b)
    | otherwise = (r, g, b)
  update (r, g, b) (Green n)
    | n > g = (r, n, b)
    | otherwise = (r, g, b)
  update (r, g, b) (Blue n)
    | n > b = (r, g, n)
    | otherwise = (r, g, b)
