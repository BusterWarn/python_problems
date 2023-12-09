import qualified Day_00.Solution as Day00
import qualified Day_01.Solution as Day01
import qualified Day_02.Solution as Day02
import qualified Day_03.Solution as Day03
import qualified Day_04.Solution as Day04
import qualified Day_05.Solution as Day05
import qualified Day_06.Solution as Day06
import qualified Day_07.Solution as Day07
import qualified Day_08.Solution as Day08
import qualified Day_09.Solution as Day09

import Test.Hspec

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  describe "Day 01 Solutions" $ do
    it "solves first" $ do
      result <- Day01.solveFirst "src/Day_01/test_input_1.txt"
      result `shouldBe` 142
    it "solves second" $ do
      result <- Day01.solveSecond "src/Day_01/test_input_2.txt"
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
  describe "day 06 solutions" $ do
    it "solves first" $ do
      result <- Day06.solveFirst "src/Day_06/test_input.txt"
      result `shouldBe` 288
    it "solves second" $ do
      result <- Day06.solveSecond "src/Day_06/test_input.txt"
      result `shouldBe` 71503
  describe "day 07 solutions" $ do
    it "Calculates hand correctly" $ do
      Day07.determineHandStrength "AAAAA" `shouldBe` 7 -- Five of a Kind
      Day07.determineHandStrength "AABAA" `shouldBe` 6 -- Four of a Kind
      Day07.determineHandStrength "A5A5A" `shouldBe` 5 -- Full House
      Day07.determineHandStrength "AACAB" `shouldBe` 4 -- Three of a Kind
      Day07.determineHandStrength "77ABB" `shouldBe` 3 -- Two Pair
      Day07.determineHandStrength "ABBCD" `shouldBe` 2 -- One Pair
      Day07.determineHandStrength "ABCDE" `shouldBe` 1 -- All Unique
    it "solves first" $ do
      result <- Day07.solveFirst "src/Day_07/test_input.txt"
      result `shouldBe` 6440
    it "solves second" $ do
      result <- Day07.solveSecond "src/Day_07/test_input.txt"
      result `shouldBe` 1
  describe "day 08 solutions" $ do
    it "solves first" $ do
      result <- Day08.solveFirst "src/Day_08/test_input.txt"
      result `shouldBe` 2
    it "solves second" $ do
      result <- Day08.solveSecond "src/Day_08/test_input.txt"
      result `shouldBe` 1
  describe "day 09 solutions" $ do
    it "solves first" $ do
      result <- Day09.solveFirst "src/Day_09/test_input.txt"
      result `shouldBe` 114
    it "solves second" $ do
      result <- Day09.solveSecond "src/Day_09/test_input.txt"
      result `shouldBe` 2
