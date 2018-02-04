module Katas.Flow.CaseExpressionSpec (spec) where

import Test.Hspec
import Test.QuickCheck

head' :: [a] -> a
head' xs = case xs of
             [] -> error "No item"
             (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
                                      [] -> "empty."
                                      (x:[]) -> "a singleton list."
                                      (x:_) -> "a longer list."

{- Case statement can be written with patten matching -}
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "is empty."
          what (x:[]) = "a singleton list."
          what (x:_) = "a longer list."

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Case expressions" $ do
        it "can be used anywhere" $ do
            head' [1,3] `shouldBe` 1
        it "can be even used in expressions" $ do
            describeList [] `shouldBe` "The list is empty."
            describeList [1] `shouldBe` "The list is a singleton list."
            describeList [1,2] `shouldBe` "The list is a longer list."
            describeList' [1] `shouldBe` "The list is a singleton list."

-- Continue to Katas.Flow.RecursionsSpec
