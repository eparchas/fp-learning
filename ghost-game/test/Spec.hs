import Data.Bool (Bool (True))
import qualified Data.ByteString.Char8 as BSU
import qualified Data.Trie as T
import qualified Data.Set as S
import Lib (calculateFirstLetters)
import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.List (sort)
import Debug.Trace (trace)

main :: IO ()
main = hspec $ do
  describe "Simple test" $ do
    it "should return single winning word" $ do
      let input = sort ["bee", "beer", "bear"]
      (calculateFirstLetters input) `shouldBe` ["bear"]
    
    it "should return empty list for no possible win" $ do
        let input = sort ["bts", "bear"]
        (calculateFirstLetters input) `shouldBe` []
      
    it "should return empty list for secondary path" $ do
        let input = sort ["bet", "betan" , "better"]
        (calculateFirstLetters input) `shouldBe` []


            