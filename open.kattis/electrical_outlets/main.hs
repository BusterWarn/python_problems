import Data.Maybe

solve_line :: [Int] -> Maybe Int
solve_line (k:[]) = Nothing
solve_line (k:os) = Just ((sum os) - k + 1)

solve :: String -> String
solve input =
  let linesAsInts = convertToListOfInts input
      output = catMaybes $ map solve_line linesAsInts
  in unlines $ map show output

convertToListOfInts :: String -> [[Int]]
convertToListOfInts input = map (map read . words) $ lines input

main :: IO ()
main = interact $
  unlines . map solvo . lines