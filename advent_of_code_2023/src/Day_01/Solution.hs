module Day_01.Solution (
  solve_1,
  solve_2,
) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import PuzzleReader (readInput)

-- Part 1

solve_1 :: FilePath -> IO [String]
solve_1 puzzle_input_file = do
  file_content <- readInput puzzle_input_file
  let numbers = map get_calibration_number_1 $ file_content
      sumNumbers = sum numbers
  return [show sumNumbers]

get_calibration_number_1 :: String -> Int
get_calibration_number_1 input_string = do
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
    , ("1", 1)
    , ("two", 2)
    , ("2", 2)
    , ("three", 3)
    , ("3", 3)
    , ("four", 4)
    , ("4", 4)
    , ("five", 5)
    , ("5", 5)
    , ("six", 6)
    , ("6", 6)
    , ("seven", 7)
    , ("7", 7)
    , ("eight", 8)
    , ("8", 8)
    , ("nine", 9)
    , ("9", 9)
    ]

solve_2 :: FilePath -> IO [String]
solve_2 puzzle_input_file = do
  file_content <- readInput puzzle_input_file
  let numbers = map get_digit_word_in_string file_content
      sumNumbers = sum numbers
  return [show sumNumbers]

get_digit_word_in_string :: String -> Int
get_digit_word_in_string str = do
  let first_digit = fromJust $ get_first_word_digit_in_string str
      second_digit = fromJust $ get_last_word_digit_in_string str
  concat_ints first_digit second_digit

concat_ints :: Int -> Int -> Int
concat_ints a b = read (show a ++ show b) :: Int

get_first_word_digit_in_string :: String -> Maybe Int
get_first_word_digit_in_string [] = Nothing
get_first_word_digit_in_string (c : cs) = do
  case sub_string of
    Nothing -> get_first_word_digit_in_string cs
    Just digit -> Just digit
 where
  sub_string = string_starts_with_digit (c : cs)

get_last_word_digit_in_string :: String -> Maybe Int
get_last_word_digit_in_string str = process_string_backwards str ""
 where
  process_string_backwards [] right_str = string_starts_with_digit right_str
  process_string_backwards left_str right_str =
    case string_starts_with_digit right_str of
      Just digit -> Just digit
      Nothing -> process_string_backwards (init left_str) (last left_str : right_str)

string_starts_with_digit :: String -> Maybe Int
string_starts_with_digit str =
  case matching_prefixes of
    [] -> Nothing
    ((_, digit) : _) -> Just digit
 where
  word_digit_pairs = Map.toList word_to_digit
  matching_prefixes = filter (\(word, _) -> word `isPrefixOf` str) word_digit_pairs
