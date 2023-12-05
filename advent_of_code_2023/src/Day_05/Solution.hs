module Day_05.Solution (
  solveFirst,
  solveSecond,
) where

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import PuzzleReader (readInput)

-- destination range start, source range start, range lengh
type Map_x2y = (Int, Int, Int)

type FromToMapping = (String, [Map_x2y])

-- >>> splitOnDoubleNewline "Hel\nlo\n\nWorld"
-- ["Hel\nlo","World"]
splitOnDoubleNewline :: String -> [String]
splitOnDoubleNewline [] = [""]
splitOnDoubleNewline ('\n' : '\n' : xs) = "" : splitOnDoubleNewline xs
splitOnDoubleNewline (x : xs) =
  let rest = splitOnDoubleNewline xs
   in (x : head rest) : tail rest

-- >>> parseMap $ "light-to-temperature map:\n45 77 23\n81 45 19\n68 64 13"
parseMap :: String -> FromToMapping
parseMap rawMap =
  let (fromTo, linesRaw) = break (== '\n') (trimNewLine rawMap)
      parsedLines = map (parseNumbers . words) . splitOn "\n" $ dropWhile (== '\n') linesRaw
   in (trimNewLine fromTo, parsedLines)
 where
  parseNumbers [x, y, z] = (read x, read y, read z)
  parseNumbers line = error $ "Invalid line format: " ++ unlines line
  trimNewLine = reverse . dropWhile (== '\n') . reverse

parseMaps :: [String] -> [FromToMapping]
parseMaps [] = []
parseMaps (mapRaw : restOfMapsRaw) =
  let newMapping = parseMap mapRaw
   in newMapping : parseMaps restOfMapsRaw

parseInput :: FilePath -> IO ([Int], [FromToMapping])
parseInput puzzleInputFile = do
  input <- readInput puzzleInputFile
  let (seeds_raw : maps_raw) = splitOnDoubleNewline $ unlines input
      seeds = map read . tail . words $ seeds_raw :: [Int]
      maps = parseMaps maps_raw
  print seeds
  print maps
  return (seeds, maps)

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  (seeds, fromToMaps) <- parseInput puzzleInputFile
  let seedsTransformed = map (\s -> pushNumberThroughMaps s fromToMaps) seeds
  print seedsTransformed
  print $ minimum seedsTransformed
  return $ minimum seedsTransformed

-- >>> pushNumberThroughMaps 14 [("seed-to-soil map:",[(50,98,2),(52,50,48)]),("soil-to-fertilizer map:",[(0,15,37),(37,52,2),(39,0,15)]),("fertilizer-to-water map:",[(49,53,8),(0,11,42),(42,0,7),(57,7,4)]),("water-to-light map:",[(88,18,7),(18,25,70)]),("light-to-temperature map:",[(45,77,23),(81,45,19),(68,64,13)]),("temperature-to-humidity map:",[(0,69,1),(1,0,69)]),("humidity-to-location map:",[(60,56,37),(56,93,4)])]
-- 43
pushNumberThroughMaps :: Int -> [FromToMapping] -> Int
pushNumberThroughMaps num [] = num
pushNumberThroughMaps num [] = trace ("return num " ++ show num ++ "\n") num
pushNumberThroughMaps num (fromToMap : rest) =
  let transformedNum = pushNumberThroughMap num fromToMap
      -- in pushNumberThroughMaps transformedNum rest
      res = pushNumberThroughMaps transformedNum rest
   in trace (fst fromToMap ++ " transforms " ++ show num ++ " to " ++ show transformedNum) res

-- >>> pushNumberThroughMap 53 ("fertilizer-to-water map:",[(49,53,8),(0,11,42),(42,0,7),(57,7,4)])
-- 49
pushNumberThroughMap :: Int -> FromToMapping -> Int
pushNumberThroughMap num (_, fromToMaps) = go num fromToMaps
 where
  go n [] = n
  go n (xTooYMap : rest) =
    let transformedNumber = pushXToY n xTooYMap
     in if transformedNumber /= n
          then transformedNumber
          else go transformedNumber rest

-- >>> pushXToY 79 (52, 50, 48)
-- 81
pushXToY :: Int -> Map_x2y -> Int
pushXToY num (destRangeStart, sourceRangeStart, rangeLength)
  | num >= sourceRangeStart && num <= (sourceRangeStart + rangeLength) =
      let delta = num - sourceRangeStart
       in destRangeStart + delta
  | otherwise = num

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  input <- readInput puzzleInputFile
  return 1

-- 81
