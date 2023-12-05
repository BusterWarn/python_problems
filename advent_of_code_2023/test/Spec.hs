import qualified Day_00.Solution as Day00
import qualified Day_01.Solution as Day01
import qualified Day_02.Solution as Day02
import qualified Day_03.Solution as Day03
import qualified Day_04.Solution as Day04
import qualified Day_05.Solution as Day05

import Test.Hspec

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  describe "Day 01 Solutions" $ do
    it "solves first" $ do
      result <- Day01.solve_1 "src/Day_01/test_input_1.txt"
      result `shouldBe` 142
    it "solves second" $ do
      result <- Day01.solve_2 "src/Day_01/test_input_2.txt"
      result `shouldBe` 281

  describe "day 02 solutions" $ do
    it "solves first" $ do
      result <- Day02.solveFirst "src/Day_02/test_input.txt"
      result `shouldBe` 8
    it "solves second" $ do
      result <- Day02.solveSecond "src/Day_02/test_input.txt"
      result `shouldBe` 2286

  describe "day 03 solutions" $ do
    it "Neightbours should be correct" $ do
      let matrix =
            [ "1230"
            , "45+0"
            , "7890"
            ]
      Day03.isElemAdjacentToSymbol ('1', 0, 0) matrix `shouldBe` False
      Day03.isElemAdjacentToSymbol ('2', 0, 1) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('3', 0, 2) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('0', 0, 3) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('4', 1, 0) matrix `shouldBe` False
      Day03.isElemAdjacentToSymbol ('5', 1, 1) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('+', 1, 2) matrix `shouldBe` False
      Day03.isElemAdjacentToSymbol ('0', 1, 3) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('7', 2, 0) matrix `shouldBe` False
      Day03.isElemAdjacentToSymbol ('8', 2, 1) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('9', 2, 2) matrix `shouldBe` True
      Day03.isElemAdjacentToSymbol ('0', 2, 3) matrix `shouldBe` True
    it "solves first" $ do
      result <- Day03.solveFirst "src/Day_03/test_input.txt"
      result `shouldBe` 4361
    it "solves second" $ do
      result <- Day03.solveSecond "src/Day_03/test_input.txt"
      result `shouldBe` 467835
  describe "day 04 solutions" $ do
    it "solves first" $ do
      result <- Day04.solveFirst "src/Day_04/test_input.txt"
      result `shouldBe` 13
    it "solves second" $ do
      result <- Day04.solveSecond "src/Day_04/test_input.txt"
      result `shouldBe` 30
  describe "day 05 solutions" $ do
    it "solves first" $ do
      result <- Day05.solveFirst "src/Day_05/test_input.txt"
      result `shouldBe` 35
    it "solves second" $ do
      result <- Day05.solveSecond "src/Day_05/test_input.txt"
      result `shouldBe` 1
