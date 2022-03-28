import Data.Bool (Bool (True))
import qualified Data.ByteString.Char8 as BSU
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Trie as T
import Debug.Trace (trace)
import Lib (calculateFirstLetters, calculateWinningSequences)
import Test.Hspec ( hspec, describe, it, shouldBe, Expectation )
import Test.QuickCheck
    ( listOf,
      listOf1,
      forAll,
      arbitraryASCIIChar,
      Arbitrary(arbitrary),
      Testable(property) )

playsToWin :: [String] -> [String] -> Expectation
playsToWin dict play = calculateFirstLetters dict `shouldBe` play

main :: IO ()
main = hspec $ do
  describe "Check general properties" $ do
    it "There should always be a winner for non empty dictionary" $
      property $ forAll (listOf $ listOf1 arbitraryASCIIChar) $ \ws -> not $ null (calculateWinningSequences ws)
    it "Order of input should not affect result" $
      property $ forAll (listOf $ listOf1 arbitraryASCIIChar) $ \ws -> calculateWinningSequences ws == calculateWinningSequences (reverse ws)
    
  describe "Check that first letters are caclulated correctly" $ do
    it "should return single winning word" $ do
      ["bee", "beer", "bear"] `playsToWin` ["b,a"]

    it "should return empty list for no possible win" $ do
      ["bts", "bear"] `playsToWin` []

    it "should return empty list for secondary path" $ do
      ["bet", "betan", "better"] `playsToWin` []

    it "should return equally winning words" $ do
      ["bear", "calf"] `playsToWin` ["b", "c"]

