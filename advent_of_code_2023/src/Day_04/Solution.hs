module Day_04.Solution (
  solveFirst,
  solveSecond,
) where

import Data.Char (isSpace)
import PuzzleReader (readInput, trimPrefixWithRegex)

-- Card Index, Copies, Winners, My Numbers
type Card = (Int, Int, [Int], [Int])

getCountOfCard :: Card -> Int
getCountOfCard (_, c, _, _) = c

getIndexOfCard :: Card -> Int
getIndexOfCard (i, _, _, _) = i

parseInput :: FilePath -> IO [Card]
parseInput puzzleInputFile = fmap (map splitAndFormatLine) (readInput puzzleInputFile)

-- >>> splitAndFormatLine "Card 78:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
-- (77,1,[1,21,53,59,44],[69,82,63,72,16,21,14,1])
splitAndFormatLine :: String -> Card
splitAndFormatLine line = do
  let (first, second) = break isColon line
      game_id = read $ filter (not . isSpace) $ trimPrefixWithRegex first "Card"
      (winning_numbers_raw, my_numbers_raw) = break isPipe second
      winning_numbers = map read $ words $ drop 1 winning_numbers_raw
      my_numbers = map read $ words $ drop 1 my_numbers_raw
  (game_id - 1, 1, winning_numbers, my_numbers)
 where
  isColon c = c == ':'
  isPipe c = c == '|'

-- >>> getNrWinnersInCard (0, 2, [41, 48, 83, 86, 17], [83, 86, 6, 31, 17, 9, 48, 53])
-- 4
getNrWinnersInCard :: Card -> Int
getNrWinnersInCard (_, _, winningNumbers, actualNumbers) =
  length . filter id $ map (\number -> isWinningNumber number winningNumbers) actualNumbers
 where
  isWinningNumber n winners = n `elem` winners

-- Part 1

solveFirst :: FilePath -> IO Int
solveFirst puzzleInputFile = do
  input <- parseInput puzzleInputFile
  return $ sum $ map (calculateWeirdScore . getNrWinnersInCard) input

-- >>> calculateWeirdScore 4
-- 8
calculateWeirdScore :: Int -> Int
calculateWeirdScore 0 = 0
calculateWeirdScore 1 = 1
calculateWeirdScore n = 2 * calculateWeirdScore (n - 1)

-- Part 2

solveSecond :: FilePath -> IO Int
solveSecond puzzleInputFile = do
  cards <- parseInput puzzleInputFile
  return $ scratchAllTheCards cards

scratchAllTheCards :: [Card] -> Int
scratchAllTheCards cards = sum $ map getCountOfCard $ go cards 0
 where
  go cs i
    | i >= length cs = cs
    | otherwise =
        let card = cs !! i
            newCards = scratchcardToScratchcards card cs
         in go newCards (i + 1)

-- >>> scratchcardToScratchcards (0, 1, [58, 59], [58, 59]) [(1, 1, [], []), (2, 1, [], []), (3, 1, [], []), (4, 1, [], [])]
-- [(1,2,[],[]),(2,2,[],[]),(3,1,[],[]),(4,1,[],[])]
scratchcardToScratchcards :: Card -> [Card] -> [Card]
scratchcardToScratchcards card cards =
  let scoreOfCard = getNrWinnersInCard card
      indicesToUpdate = generateIndiceToUpdateList (getIndexOfCard card) scoreOfCard
   in incrementCardsAtIndices indicesToUpdate cards (getCountOfCard card)

-- >>> generateIndiceToUpdateList 3 10
-- [4,5,6,7,8,9,10,11,12,13]
generateIndiceToUpdateList :: Int -> Int -> [Int]
generateIndiceToUpdateList index numbers = [index + 1 .. index + numbers]

-- >>> incrementCardsAtIndices [1, 2] [(0, 0, [0, 0], [1]), (1, 0, [4], [3, 3]), (2, 0, [17, 51], [21, 3])] 2
-- [(0,0,[0,0],[1]),(1,2,[4],[3,3]),(2,2,[17,51],[21,3])]
incrementCardsAtIndices :: [Int] -> [Card] -> Int -> [Card]
incrementCardsAtIndices indices cards copies = map incrementAtIndex cards
 where
  incrementAtIndex (index, c, winners, numbers)
    | index `elem` indices = (index, c + copies, winners, numbers)
    | otherwise = (index, c, winners, numbers)
