module Katas.Monads.DoNotationSpec (spec) where

import Test.Hspec
import Test.QuickCheck

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- Use do expression
fooMonad :: Maybe String
fooMonad = do
    let x = 3
        y = "!"
    Just (show x ++ y)

-- Use do expression here as well
marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "The do notation" $ do
        it "can combine functions with Maybe values" $ do
            -- Use a lambda here
            (Just 3 >>= (\x -> Just (show x ++ "!")))
                `shouldBe` Just "3!"
            (Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
                `shouldBe` Just "3!"
            (Just 3 >>= (\x -> Just "!" >>= const Nothing))
                `shouldBe` (Nothing :: Maybe String)
        it "can be wrapped on different lines" $ do
            foo `shouldBe` Just "3!"
            fooMonad `shouldBe` Just "3!"
        it "can simplify more complex expressions" $ do
            -- Use a lambda here for the predicate
            (Just 9 >>= (\x -> Just (x > 8))) `shouldBe` Just True
            marySue `shouldBe` Just True
