{- | This module provides functionality to read and process puzzle inputs.
  It includes functions to read from a file and process the content
  into a desired format, such as a list of strings.
-}
module PuzzleReader (
  readInput,
  trimPrefixWithRegex,
) where

import Data.List (dropWhileEnd)
import Text.Regex (mkRegex, mkRegexWithOpts, subRegex)

{- readInput
  Reads the content of a file and processes it into a list of strings.
  Each line of the file becomes an element in the list. Trailing empty lines
  are trimmed.

  Usage example:

  > input <- readInput "path/to/file.txt"
  > print input

  This will read the contents of 'file.txt' and print them as a list of strings.
-}
readInput :: FilePath -> IO [String]
readInput path = do
  content <- readFile path
  return $ filterLines $ lines content
 where
  filterLines = dropWhileEnd null

{- | trimPrefixWithRegex
Description: Removes a prefix from a string based on a regular expression.
Parameters:
  inputLine: The string from which the prefix will be removed.
  regex: The regular expression defining the prefix to remove.
Returns: The input string with the prefix defined by the regex removed.
If the regex does not match at the beginning of the string, the original string is returned unchanged.
Examples:
>>> trimPrefixWithRegex "Game 68: green 3" "Game [0-9]+: "
"green 3"
-}
trimPrefixWithRegex :: String -> String -> String
trimPrefixWithRegex str regex = subRegex (mkRegex regex) str ""

{- | trimSuffixWithRegex
Description: Removes a suffix from a string based on a regular expression.
Parameters:
  inputLine: The string from which the suffix will be removed.
  regex: The regular expression defining the suffix to remove.
Returns: The input string with the suffix defined by the regex removed.
If the regex does not match at the end of the string, the original string is returned unchanged.
Examples:
-}

-- >>> trimSuffixWithRegex $ "soil-to-fertilizer map:" " map."
trimSuffixWithRegex :: String -> String -> String
trimSuffixWithRegex str regex =
  subRegex (mkRegexWithOpts (regex ++ "$") False True) str ""
