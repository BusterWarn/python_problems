module Day_01.Solution (
  solve_1,
  solve_2,
  get_all_digit_words_in_string,
) where

import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe)
import PuzzleReader (read_input)

-- Part 1

solve_1 :: FilePath -> IO [String]
solve_1 puzzle_input_file = do
  file_content <- read_input puzzle_input_file
  let numbers = map get_calibration_number_1 $ file_content
      sumNumbers = sum numbers
  return [show sumNumbers]

get_calibration_number_1 :: String -> Int
get_calibration_number_1 input_string =
  let first_calibration_number = fromJust . get_first_digit_in_string $ input_string
      second_calibration_number = fromJust . get_first_digit_in_string . reverse $ input_string
   in read $ [first_calibration_number, second_calibration_number]

get_first_digit_in_string :: String -> Maybe Char
get_first_digit_in_string [] = Nothing
get_first_digit_in_string (x : xs) =
  case filter_digit x of
    Just result -> Just result
    Nothing -> get_first_digit_in_string xs

filter_digit :: Char -> Maybe Char
filter_digit c
  | c >= '0' && c <= '9' = Just c
  | otherwise = Nothing

-- Part two

word_to_digit :: Map.Map String Int
word_to_digit =
  Map.fromList
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

solve_2 :: FilePath -> IO [String]
solve_2 puzzle_input_file = do
  file_content <- read_input puzzle_input_file
  let numbers = map get_calibration_number_2 file_content
      sumNumbers = sum numbers
  return [show sumNumbers]

get_calibration_number_2 :: String -> Int
get_calibration_number_2 input_string =
  let first_calibration_number = fromJust . get_first_digit_in_string $ input_string
      second_calibration_number = fromJust . get_first_digit_in_string . reverse $ input_string
   in read [first_calibration_number, second_calibration_number]

find_digit_in_string :: String -> Int
find_digit_in_string = undefined

-- >>> get_all_digit_words_in_string $ "one1123tttwo99jfjffivee41"
-- ["one", "two", "five"]
get_all_digit_words_in_string :: String -> [String]
get_all_digit_words_in_string str = filter (`isInfixOf` str) (Map.keys word_to_digit)
