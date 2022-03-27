import Data.Bool (Bool (True))
import qualified Data.ByteString.Char8 as BSU
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Trie as T
import Lib (calculateFirstLetters)
import Test.Hspec (describe, hspec, it, shouldBe, Expectation)

playsToWin :: [String] -> [String] -> Expectation 
playsToWin dict play = calculateFirstLetters dict `shouldBe` play

main :: IO ()
main = hspec $ do
  describe "Check that first letters are caclulated correctly" $ do
    it "should return single winning word" $ do
      ["bee", "beer", "bear"] `playsToWin` ["b,a"]

    it "should return empty list for no possible win" $ do
      ["bts", "bear"] `playsToWin` []

    it "should return empty list for secondary path" $ do
      ["bet", "betan", "better"] `playsToWin` []
    
    it "should return equally winning words" $ do
        ["bear", "calf"] `playsToWin` ["b", "c"]
    
{- |
Properties
- There should be a winner for every non zero length input
- Order of input should not change who the winner is
-}