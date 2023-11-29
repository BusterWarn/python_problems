-- | This module provides functionality to read and process puzzle inputs.
--   It includes functions to read from a file and process the content
--   into a desired format, such as a list of strings.
module PuzzleReader 
  ( read_input
  ) where

import Data.List (dropWhileEnd)

-- | Reads the content of a file and processes it into a list of strings.
--   Each line of the file becomes an element in the list. Trailing empty lines
--   are trimmed.
-- 
--   Usage example:
-- 
--   > input <- read_input "path/to/file.txt"
--   > print input
-- 
--   This will read the contents of 'file.txt' and print them as a list of strings.
--
read_input :: FilePath -> IO [String]
read_input path = do
  content <- readFile path
  return $ filterLines $ lines content
  where
    filterLines = dropWhileEnd null
