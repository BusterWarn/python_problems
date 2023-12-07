module Day_06.Solution (
  solveFirst,
  solveSecond,
) where

import PuzzleReader (readInput, trimPrefixWithRegex)

-- Your existing functions here

convertToInts :: String -> [Int]
convertToInts = map read . words

processInputProblemOne :: FilePath -> IO [(Int, Int)]
processInputProblemOne path = do
    inputLines <- readInput path
    let timeLine = head inputLines
        distanceLine = inputLines !! 1
        times = convertToInts $ trimPrefixWithRegex timeLine "Time:\\s+"
        distances = convertToInts $ trimPrefixWithRegex distanceLine "Distance:\\s+"
    return $ zip times distances

processInputProblemTwo :: FilePath -> IO (Int, Int)
processInputProblemTwo path = do
    inputLines <- readInput path
    let timeLine = head inputLines
        distanceLine = inputLines !! 1
        time = read $ filter (/= ' ') $ trimPrefixWithRegex timeLine "Time:\\s+"
        distance = read $ filter (/= ' ') $ trimPrefixWithRegex distanceLine "Distance:\\s+"
    return (time, distance)

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  input <- processInputProblemOne  puzzleInputFile
  return $ foldl (*) 1 $ map (\(time, record) -> getNumberOfWaysToBeatRecord time record) input

getNumberOfWaysToBeatRecord :: Int -> Int -> Int
getNumberOfWaysToBeatRecord time record =
  let iterations = [1 .. time]
      lengths = map (\i -> holdTheButtonAndGoThisFar i time) iterations
  in length $ filter (> record) lengths


holdTheButtonAndGoThisFar :: Int -> Int -> Int
holdTheButtonAndGoThisFar speed time
  | speed >= time = 0
  | otherwise = 
    let timeToGo = time - speed
    in timeToGo * speed


-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  (time, record) <- processInputProblemTwo puzzleInputFile
  return $ getNumberOfWaysToBeatRecord time record

-- 81
