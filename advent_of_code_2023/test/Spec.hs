import Test.Hspec
import qualified Day_00.Solution as Day00

main :: IO ()
main = hspec tests

tests :: Spec
tests = do
  describe "Day 00 Solution" $ do
    it "correctly solves the first part" $ do
      result <- Day00.solve "src/Day_00/test_input.txt"
      result `shouldBe` ["Hello", "World!"]
    -- other tests
