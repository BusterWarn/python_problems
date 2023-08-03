solve :: String -> String
solve (c1:c2:[]) = [c2, c1]
solve input = error $ "Invalid Input: \"" ++ input ++ "\""

main :: IO ()
main = interact $
  unlines . map solve . lines