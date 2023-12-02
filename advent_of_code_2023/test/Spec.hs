import qualified Day_00.Solution as Day00
import qualified Day_01.Solution as Day01
import qualified Day_02.Solution as Day02

import Test.Hspec

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  describe "Day 00 Solution" $ do
    it "correctly solves the first part" $ do
      result <- Day00.solve "src/Day_00/test_input.txt"
      result `shouldBe` ["Hello", "World!"]

  describe "Day 01 Solutions" $ do
    it "solves first" $ do
      result <- Day01.solve_1 "src/Day_01/test_input_1.txt"
      result `shouldBe` ["142"]
    it "solves second" $ do
      result <- Day01.solve_2 "src/Day_01/test_input_2.txt"
      result `shouldBe` ["281"]

  describe "day 02 solutions" $ do
    it "solves first" $ do
      result <- Day02.solveFirst "src/day_02/test_input.txt"
      result `shouldBe` ["8"]
    it "solves second" $ do
      result <- Day02.solveSecond "src/day_02/test_input.txt"
      result `shouldBe` ["2286"]
