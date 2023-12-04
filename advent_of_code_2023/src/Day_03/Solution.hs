module Day_03.Solution (
  solveFirst,
  solveSecond,
  isElemAdjacentToSymbol,
) where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import PuzzleReader (readInput)

-- Common code for both problems

type Matrix a = [[a]]
type Elem a = (a, Int, Int)

getElement :: Int -> Int -> Matrix Char -> Maybe (Elem Char)
getElement x y matrix =
  if validIndex x (length matrix) && validIndex y (length (head matrix))
    then Just ((matrix !! x) !! y, x, y)
    else Nothing

validIndex :: Int -> Int -> Bool
validIndex i size = i >= 0 && i < size

getDigitAsList :: Int -> Int -> Matrix Char -> [Elem Char]
getDigitAsList x y matrix = go x y matrix []
 where
  go _ ny _ acc =
    -- Only care about new x here
    case getElement x ny matrix of
      Nothing -> reverse acc
      Just (c, _, _) ->
        if isCharInt c
          then go x (ny + 1) matrix ((c, x, ny) : acc)
          else reverse acc

valueOfDigitAsList :: [Elem Char] -> Int
valueOfDigitAsList elements = read $ go elements []
 where
  go [] acc = reverse acc
  go ((c, _, _) : rest) acc = go rest (c : acc)

getListOfAdjacentCoords :: Int -> Int -> [(Int, Int)]
getListOfAdjacentCoords x y =
  -- Above
  [ (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , -- Middle
    (x, y - 1)
  , (x, y + 1)
  , -- Below
    (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]

getNeighbours :: Int -> Int -> Matrix Char -> [Elem Char]
getNeighbours x y matrix =
  let coords = getListOfAdjacentCoords x y
   in catMaybes $ map (\(nx, ny) -> getElement nx ny matrix) coords

isCharInt :: Char -> Bool
isCharInt c = c `elem` ['0' .. '9']

isBeginningOfDigit :: Elem Char -> Matrix Char -> Bool
isBeginningOfDigit (c, x, y) matrix = do
  isCharInt c
    && ( case getElement x (y - 1) matrix of
          Nothing -> True
          Just (n, _, _) -> not $ isCharInt n
       )

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  matrix <- readInput puzzleInputFile
  let coordsMatrix = [((matrix !! x) !! y, x, y) | x <- [0 .. length matrix - 1], y <- [0 .. length (head matrix) - 1]]
  return $ sum [getValueOfPartNumber element matrix | element <- coordsMatrix]

-- If element in the matrix is part number then return the value
-- Only valid for first entry in digit.
getValueOfPartNumber :: Elem Char -> Matrix Char -> Int
getValueOfPartNumber (c, x, y) matrix
  | not $ isBeginningOfDigit (c, x, y) matrix = 0
  | otherwise =
      let digitAsList = getDigitAsList x y matrix
          isElementPartNumber = any elemento digitAsList
       in if isElementPartNumber
            then valueOfDigitAsList digitAsList
            else 0
 where
  elemento nc = isElemAdjacentToSymbol nc matrix

isElemAdjacentToSymbol :: Elem Char -> Matrix Char -> Bool
isElemAdjacentToSymbol (_, x, y) matrix =
  let neighbours = getNeighbours x y matrix
      symbols = filter isNotDigitNorDot neighbours
   in not . null $ symbols
 where
  isNotDigitNorDot (c, _, _) = not (isCharInt c || c == '.')

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  matrix <- readInput puzzleInputFile
  let coordsMatrix = [((matrix !! x) !! y, x, y) | x <- [0 .. length matrix - 1], y <- [0 .. length (head matrix) - 1]]
  return $ sum [getValueOfGear element matrix | element <- coordsMatrix]

getValueOfGear :: Elem Char -> Matrix Char -> Int
getValueOfGear (c, x, y) matrix
  | c /= '*' = 0
  | otherwise =
      let adjacentPartNumbers = getAdjacentPartNumbers x y matrix
       in if length adjacentPartNumbers == 2
            then product adjacentPartNumbers
            else 0

getAdjacentPartNumbers :: Int -> Int -> Matrix Char -> [Int]
getAdjacentPartNumbers x y matrix =
  let neighbours = getNeighbours x y matrix
      values = catMaybes $ map (\(_, nx, ny) -> getDigitAndCoord nx ny matrix) neighbours
      uniqueValues = Map.fromList values
   in Map.elems uniqueValues

getDigitAndCoord :: Int -> Int -> Matrix Char -> Maybe ((Int, Int), Int)
getDigitAndCoord x y matrix = do
  case getElement x y matrix of
    Nothing -> Nothing
    Just (c, _, _) ->
      if isCharInt c
        then
          if isBeginningOfDigit (c, x, y) matrix
            then Just ((x, y), valueOfDigitAsList $ getDigitAsList x y matrix)
            else getDigitAndCoord x (y - 1) matrix
        else Nothing
