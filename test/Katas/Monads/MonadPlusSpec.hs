module Katas.Monads.MonadPlusSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad

{-
    MonadPlus type class is for monads that can also act as monoids:

    class Monad m => MonadPlus m where
        mzero :: m a
        mplus :: m a -> m a -> m a

    instance MonadPlus [] where
        mzero = []
        mplus = (++)

    guard :: (MonadPlus m) => Bool -> m ()
    guard True = return ()
    guard False = mzero
-}

-- Use do notation just like in the examples below
sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "guard" $ do
        it "can filter list of numbers with digits 7" $
            [x | x <- [1..50], '7' `elem` show x]
                `shouldBe` [7,17,27,37,47]
        it "can put it in a minimal default context if true" $ do
            -- compare 5 and 2 in the examples below
            (guard (5 > 2) :: Maybe ()) `shouldBe` Just ()
            (guard (1 > 2) :: Maybe ()) `shouldBe` Nothing
            (guard (5 > 2) :: [()]) `shouldBe` [()]
            (guard (1 > 2) :: [()]) `shouldBe` []
        it "can be used to filter out non-deterministic computations" $
            ([1..50] >>= (\x -> guard ('7' `elem` show x) >> return x))
                `shouldBe` [7,17,27,37,47]
        it "works in conjunction with >>" $ do
            -- compare 5 and 2 in the examples below
            (guard (5 > 2) >> return "cool" :: [String])
                `shouldBe` ["cool"]
            (guard (1 > 2) >> return "cool" :: [String])
                `shouldBe` []
        it "can be expressed with do notation" $
            sevensOnly `shouldBe` [7,17,27,37,47]
