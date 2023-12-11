module Day_11.Solution (
  solveFirst,
  solveSecond,
) where

import Data.List (foldl', transpose)
import qualified Data.Map as Map
import PuzzleReader (readInput)

import Data.List (intercalate)
import qualified Data.Map as Map

printDistanceMap :: DistanceMap -> IO ()
printDistanceMap distanceMap = putStrLn $ formatDistanceMap distanceMap

formatDistanceMap :: DistanceMap -> String
formatDistanceMap distanceMap =
  intercalate "\n" $ map formatEntry (Map.toList distanceMap)

formatEntry :: (((Char, Int, Int), (Char, Int, Int)), Int) -> String
formatEntry (((char1, x1, y1), (char2, x2, y2)), distance) =
  concat
    [ "Distance from "
    , show (charToIndex char1)
    , " ("
    , show x1
    , ","
    , show y1
    , ") to "
    , show (charToIndex char2)
    , " ("
    , show x2
    , ","
    , show y2
    , "): "
    , show distance
    ]

charToIndex :: Char -> Int
charToIndex ch = fromEnum ch - fromEnum 'a' + 1

-- Common code for both problems

type Matrix = [[Elem]]
type Elem = (Char, Int, Int)
type DistanceMap = Map.Map (Elem, Elem) Int

indexSpace :: [[Char]] -> Matrix
indexSpace space = [[((space !! x) !! y, x, y) | y <- [0 .. length (head space) - 1]] | x <- [0 .. length space - 1]]

expandGalaxyRowWise :: [[Char]] -> [[Char]]
expandGalaxyRowWise space = go space []
 where
  go [] acc = acc
  go (row : rest) acc
    | all (== '.') row = go rest (acc ++ [row, row])
    | otherwise = go rest (acc ++ [row])

expandGalaxyColumnWise :: [[Char]] -> [[Char]]
expandGalaxyColumnWise = transpose . expandGalaxyRowWise . transpose

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  space <- readInput puzzleInputFile
  let spaceExpanded = expandGalaxyColumnWise . expandGalaxyRowWise $ space
      spaceOrganized = indexSpace spaceExpanded
      allTheGalaxies = exploreGalaxies spaceOrganized
      distancesMapped = buildDistanceMap spaceOrganized allTheGalaxies
  print "spaceOrganized"
  print spaceOrganized
  print "allTheGalaxies"
  print allTheGalaxies
  print "distance"
  printDistanceMap distancesMapped
  print "length distance"
  print $ length distancesMapped
  return $ Map.foldl' (+) 0 distancesMapped

buildDistanceMap :: Matrix -> [Elem] -> DistanceMap
buildDistanceMap matrix galaxies = foldl' addDistance Map.empty [(g1, g2) | g1 <- galaxies, g2 <- galaxies, g1 /= g2]
 where
  addDistance acc (from, to) =
    case getValueFromMem acc from to of
      Just _ -> acc -- Distance already computed
      Nothing -> Map.insert (from, to) (manhattanDistance from to matrix) acc

getValueFromMem :: DistanceMap -> Elem -> Elem -> Maybe Int
getValueFromMem mem from to =
  case Map.lookup (from, to) mem of
    Just distance -> Just distance
    Nothing -> Map.lookup (to, from) mem

-- calculateDistance :: Elem -> Elem -> Matrix -> Int
-- calculateDistance (_, fromX, fromY) (_, toX, toY) space =
--   let nrRows = length space
--       nrCols = length $ head space
--       minX = min fromX toX
--       maxX = max fromX toX
--       minY = min fromY toY
--       maxY = max fromY toY
--       distanceX = max (maxX - minX) (minX + nrRows - maxX)
--       distanceY = max (maxY - minY) (minY + nrCols - maxY)
--    in distanceX + distanceYcalculateDistance :: Elem -> Elem -> Matrix -> Int

manhattanDistance :: Elem -> Elem -> Matrix -> Int
manhattanDistance (_, fromX, fromY) (_, toX, toY) space =
  let distanceX = abs (fromX - toX)
      distanceY = abs (fromY - toY)
   in distanceX + distanceY

exploreGalaxies :: Matrix -> [Elem]
exploreGalaxies space = zipWith assignIndex galaxies ['a' ..]
 where
  galaxies = [(x, y) | (row, x) <- zip space [0 ..], ((ch, _, _), y) <- zip row [0 ..], ch == '#']
  assignIndex (x, y) idx = (idx, x, y)

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  space <- readInput puzzleInputFile
  return $ 1
