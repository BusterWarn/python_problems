module Day_10.Solution (
  solveFirst,
  solveSecond,
) where

import Data.Maybe (catMaybes, mapMaybe)
import PuzzleReader (readInput)

-- Common code for both problems

type Matrix = [[Elem]]
type Elem = (Char, Int, Int)

parseInput :: FilePath -> IO Matrix
parseInput puzzleInputFile = do
  matrix <- readInput puzzleInputFile
  return [[((matrix !! x) !! y, x, y) | y <- [0 .. length (head matrix) - 1]] | x <- [0 .. length matrix - 1]]

getElement :: Int -> Int -> Matrix -> Maybe Elem
getElement x y matrix =
  if validIndex x (length matrix) && validIndex y (length $ head matrix)
    then Just (matrix !! x !! y)
    else Nothing

validIndex :: Int -> Int -> Bool
validIndex i size = i >= 0 && i < size

-- >>> isLeftConnected ('-',1,2) [[('.',0,0),('.',0,1),('.',0,2),('.',0,3),('.',0,4)],[('.',1,0),('S',1,1),('-',1,2),('7',1,3),('.',1,4)],[('.',2,0),('|',2,1),('.',2,2),('|',2,3),('.',2,4)],[('.',3,0),('L',3,1),('-',3,2),('J',3,3),('.',3,4)],[('.',4,0),('.',4,1),('.',4,2),('.',4,3),('.',4,4)]]
-- Just ('S',1,1)
isLeftConnected :: Elem -> Matrix -> Maybe Elem
isLeftConnected (thisPipe, x, y) matrix
  | thisPipe `elem` ['|', 'F', 'L'] = Nothing
  | otherwise = case getElement x (y - 1) matrix of
      Nothing -> Nothing
      Just (c, nx, ny) ->
        case c of
          '-' -> Just (c, nx, ny)
          'L' -> Just (c, nx, ny)
          'F' -> Just (c, nx, ny)
          'S' -> case isRightConnected (c, nx, ny) matrix of
            Nothing -> Nothing
            Just _ -> Just (c, nx, ny)
          _ -> Nothing

-- >>> isAboveConnected ('|',2,1) [[('.',0,0),('.',0,1),('.',0,2),('.',0,3),('.',0,4)],[('.',1,0),('S',1,1),('-',1,2),('7',1,3),('.',1,4)],[('.',2,0),('|',2,1),('.',2,2),('|',2,3),('.',2,4)],[('.',3,0),('L',3,1),('-',3,2),('J',3,3),('.',3,4)],[('.',4,0),('.',4,1),('.',4,2),('.',4,3),('.',4,4)]]
-- Just ('S',1,1)
isAboveConnected :: Elem -> Matrix -> Maybe Elem
isAboveConnected (thisPipe, x, y) matrix
  | thisPipe `elem` ['-', '7', 'F'] = Nothing
  | otherwise = case getElement (x - 1) y matrix of
      Nothing -> Nothing
      Just (c, nx, ny) -> case c of
        '|' -> Just (c, nx, ny)
        '7' -> Just (c, nx, ny)
        'F' -> Just (c, nx, ny)
        'S' -> case isBelowConnected (c, nx, ny) matrix of
          Nothing -> Nothing
          Just _ -> Just (c, nx, ny)
        _ -> Nothing

-- >>> isRightConnected ('S',1,1) [[('.',0,0),('.',0,1),('.',0,2),('.',0,3),('.',0,4)],[('.',1,0),('S',1,1),('-',1,2),('7',1,3),('.',1,4)],[('.',2,0),('|',2,1),('.',2,2),('|',2,3),('.',2,4)],[('.',3,0),('L',3,1),('-',3,2),('J',3,3),('.',3,4)],[('.',4,0),('.',4,1),('.',4,2),('.',4,3),('.',4,4)]]
-- Just ('-',1,2)
isRightConnected :: Elem -> Matrix -> Maybe Elem
isRightConnected (thisPipe, x, y) matrix
  | thisPipe `elem` ['|', '7', 'J'] = Nothing
  | otherwise = case getElement x (y + 1) matrix of
      Nothing -> Nothing
      Just (c, nx, ny) ->
        case c of
          '-' -> Just (c, nx, ny)
          '7' -> Just (c, nx, ny)
          'J' -> Just (c, nx, ny)
          'S' -> case isLeftConnected (c, nx, ny) matrix of
            Nothing -> Nothing
            Just _ -> Just (c, nx, ny)
          _ -> Nothing

-- >>> isBelowConnected ('|',2,1) [[('.',0,0),('.',0,1),('.',0,2),('.',0,3),('.',0,4)],[('.',1,0),('S',1,1),('-',1,2),('7',1,3),('.',1,4)],[('.',2,0),('|',2,1),('.',2,2),('|',2,3),('.',2,4)],[('.',3,0),('L',3,1),('-',3,2),('J',3,3),('.',3,4)],[('.',4,0),('.',4,1),('.',4,2),('.',4,3),('.',4,4)]]
-- Just ('L',3,1)
isBelowConnected :: Elem -> Matrix -> Maybe Elem
isBelowConnected (thisPipe, x, y) matrix
  | thisPipe `elem` ['-', 'L', 'J'] = Nothing
  | otherwise = case getElement (x + 1) y matrix of
      Nothing -> Nothing
      Just (c, nx, ny) ->
        case c of
          '|' -> Just (c, nx, ny)
          'J' -> Just (c, nx, ny)
          'L' -> Just (c, nx, ny)
          'S' -> case isAboveConnected (c, nx, ny) matrix of
            Nothing -> Nothing
            Just _ -> Just (c, nx, ny)
          _ -> Nothing

-- >>> getConnectingPipes ('-',1,2) [[('.',0,0),('.',0,1),('.',0,2),('.',0,3),('.',0,4)],[('.',1,0),('S',1,1),('-',1,2),('7',1,3),('.',1,4)],[('.',2,0),('|',2,1),('.',2,2),('|',2,3),('.',2,4)],[('.',3,0),('L',3,1),('-',3,2),('J',3,3),('.',3,4)],[('.',4,0),('.',4,1),('.',4,2),('.',4,3),('.',4,4)]]
-- [('S',1,1),('7',1,3)]
getConnectingPipes :: Elem -> Matrix -> [Elem]
getConnectingPipes pipe matrix =
  let connections =
        catMaybes
          [ isLeftConnected pipe matrix
          , isRightConnected pipe matrix
          , isAboveConnected pipe matrix
          , isBelowConnected pipe matrix
          ]
   in case length connections of
        2 -> connections
        _ -> error $ "Invalid number of connections for " ++ show pipe ++ ". Found: " ++ show connections

findStartCoordinates :: Matrix -> (Int, Int)
findStartCoordinates matrix =
  case [ (rowIndex, colIndex) | (rowIndex, row) <- zip [0 ..] matrix, (colIndex, (char, _, _)) <- zip [0 ..] row, char == 'S'
       ] of
    [] -> error "No 'S' found in the matrix!"
    ((x, y) : _) -> (x, y)

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  matrix <- parseInput puzzleInputFile
  let start = findStart matrix
      pipesNavigated = crawlThroughDirtyPipes start matrix
  return $ length pipesNavigated `div` 2
 where
  findStart m =
    let (x, y) = findStartCoordinates m
     in ('S', x, y)

crawlThroughDirtyPipes :: Elem -> Matrix -> [Elem]
crawlThroughDirtyPipes start matrix = crawl [start]
 where
  crawl acc
    | length acc > 1 && last acc == start = reverse . drop 1 . reverse $ acc
    | otherwise =
        let lastTwoConnectedPipes = take 2 . reverse $ acc
            connectingPipes = getConnectingPipes (last acc) matrix
            newPipe = filterNewPipes connectingPipes lastTwoConnectedPipes
         in crawl (acc ++ [newPipe])
  filterNewPipes newPipes lastTwo -- One of the new pipes will already exist in acc
    | head newPipes `elem` lastTwo = last newPipes
    | otherwise = head newPipes

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  matrix <- readInput puzzleInputFile
  print $ zip [0 .. 0] matrix
  return 1
