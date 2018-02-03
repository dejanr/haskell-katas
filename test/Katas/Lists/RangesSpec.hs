module Katas.Lists.RangesSpec (spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ranges" $ do
        it "can express ranges" $ do
            length [1..10] `shouldBe` 10
        it "can set steps" $ do
            take 3 [2,4..] `shouldBe` [2,4,6]
    describe "cycle" $ do
        it "can cycle through numbers" $ do
            take 5 (cycle [1,2,3]) `shouldBe` [1,2,3,1,2]
    describe "repeat" $ do
        it "can repeat numbers" $ do
            take 5 (repeat 3) `shouldBe` [3,3,3,3,3]
    describe "elem" $ do
        it "can remove non-uppercase letters" $ do
            let removeNonUppercase st = [x | x <- st, x `elem` ['A'..'Z']]
            removeNonUppercase "IdontLIKEFROGS" `shouldBe` "ILIKEFROGS"

-- Continue to Katas.Lists.TuplesSpec
