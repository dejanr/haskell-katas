module Katas.Exercises.LongestZerosSpec (spec) where

import Test.Hspec
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

main :: IO ()
main = hspec spec

toBinaryString :: Int -> String
toBinaryString x = showIntAtBase 2 intToDigit x ""

longestZeros :: Int -> Int
longestZeros x = highest $ findLongestZeros (toBinaryString x)
    where highest (j, k) = if j > k then j else k
          findLongestZeros = foldl (zeroFinder) (0,0)
          zeroFinder (j, k) '1' = if j > k then (k,j) else (0,k)
          zeroFinder (j, k) '0' = (j+1, k)

spec :: Spec
spec =
    describe "Longest Zeros" $ do
        it "reports when no zero is found" $ do
            longestZeros 3 `shouldBe` 0
        it "reports one set of zero length" $ do
            longestZeros 4 `shouldBe` 2
            longestZeros 16 `shouldBe` 4
        it "reports the longest zero from multiple sets of zeros" $ do
            longestZeros 116 `shouldBe` 2
            longestZeros 11653294 `shouldBe` 4
            longestZeros 11653293525500000 `shouldBe` 9
