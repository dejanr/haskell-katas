module Katas.Exercises.RPNCalculatorSpec (spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

foldingFunction :: (Num a, Read a) => [a] -> String -> [a]
foldingFunction (x:y:xs) "+" = (x + y) : xs
foldingFunction (x:y:xs) "*" = (x * y) : xs
foldingFunction (x:y:xs) "-" = (y - x) : xs
foldingFunction xs x = (read x) : xs

solveRPN :: (Num a, Read a) => String -> a
solveRPN input = head $ foldl foldingFunction [] $ words input

spec :: Spec
spec = do
    describe "reverse polish notation" $ do
        it "uses a foldingFunction" $ do
            foldl foldingFunction [] ["1","2","+"] `shouldBe` [3]
            foldl foldingFunction [] ["2","3","*"] `shouldBe` [6]
        it "calculates simple addition" $ do
            solveRPN "1 2 +" `shouldBe` 3
        it "calculates simple multiplication" $ do
            solveRPN "2 3 *" `shouldBe` 6
        it "calculates more complex expressions" $ do
            solveRPN "10 4 3 + 2 * -" `shouldBe` (-4)
