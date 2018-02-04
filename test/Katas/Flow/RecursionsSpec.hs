module Katas.Flow.RecursionsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

maximum' :: (Ord a) => [a] -> a
maximum' (x:[]) = x
maximum' (x:xs) = x `max` (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y
                     then True
                     else myElem x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = lowersorted ++ [x] ++ highersorted
    where lowersorted = quicksort [a | a <- xs, a <= x]
          highersorted = quicksort [a | a <- xs, a > x]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Recursion" $ do
        it "calculates maximum" $ do
            maximum' [2,5,1] `shouldBe` 5
        it "replicates items" $ do
            replicate' 5 'a' `shouldBe` "aaaaa"
        it "takes from a collection" $ do
            take' 3 "abcde" `shouldBe` "abc"
        it "reverses a collection" $ do
            reverse' [1,2,3] `shouldBe` [3,2,1]
        it "can repeat items" $ do
            take' 3 (repeat' 'a') `shouldBe` "aaa"
        it "can zip items" $ do
            zip' [1,2,3] ['a','b'] `shouldBe` [(1,'a'),(2,'b')]
        it "can check if an item is an element of a list" $ do
            myElem 3 [1,2,3] `shouldBe` True
        it "can do QuickSort - easily" $ do
            quicksort [3,1,2] `shouldBe` [1,2,3]
            quicksort "attila" `shouldBe` "aailtt"

-- Continue to Katas.Hofs.IntroSpec
