import Lib (maxProfit)
import Test.Hspec (Expectation, describe, hspec, it, shouldBe)
import Test.QuickCheck (Testable (property), getPositive, listOf, forAll, Positive (Positive), Arbitrary (arbitrary), arbitrarySizedNatural, NonEmptyList (NonEmpty))

prop_increasedProfits :: Positive Int -> NonEmptyList (Positive Int) -> Bool
prop_increasedProfits (Positive fee) (NonEmpty pps) = maxProfit fee pints <= maxProfit fee long where
    pints = map getPositive pps
    long = pints ++ [fee + 1]

prop_sumOfAscendingPrices :: Positive Int -> Positive Int -> Bool
prop_sumOfAscendingPrices (Positive maxPrice) (Positive repl) = maxProfit 0 sub * repl == maxProfit 0 long where
    sub = [0..maxPrice]
    long = concat $ replicate repl sub

main :: IO ()
main = hspec $ do
    describe "Check general properties" $ do
      it "Should return increased profits for a sequence of prices and the same sequence plus a larger than cost end element" $
        property prop_increasedProfits
      it "Should return the sum of individual profits for concatenated increasing prices" $
        property prop_sumOfAscendingPrices

    describe "Check boundary conditions and valid price ranges" $ do
      it "Should return no profit for empty price list" $ do
        maxProfit 0 [] `shouldBe` 0
      it "Should return no profit for constant price independently of fees" $ do
        maxProfit 0 (replicate 20 1) `shouldBe` 0
        maxProfit 1 (replicate 20 100) `shouldBe` 0
      it "Should return no profit for descending price" $ do
        maxProfit 0 [100, 99 .. 1] `shouldBe` 0
      it "Should return no profit for ascending price where ascend does not exceed cost" $ do
        maxProfit 10 [1 .. 10] `shouldBe` 0
      it "Should return profit equal to the total ascend when there is no cost" $ do
        maxProfit 0 [1 .. 10] `shouldBe` 9
      
        