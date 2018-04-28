module Katas.Exercises.SetNeighborsSpec (spec) where

import Test.Hspec

import Data.List
import qualified Data.Matrix as M

main :: IO ()
main = hspec spec

{-
 -- Start with matrix of integers
 -- If it is a 1 on input all cardinal adjacent slots (NSEW) should become 1

matrix = [
 [ 1, 0, 0, 0, 1 ],
 [ 1, 0, 0, 1, 0 ],
 [ 0, 0, 0, 0, 0 ]
]
-}

matrix :: [[Int]]
matrix =
    [[ 1, 0, 0, 0, 1 ],
     [ 1, 0, 0, 1, 0 ],
     [ 0, 0, 0, 0, 0 ]]

updateN :: Int -> Int -> M.Matrix Int -> M.Matrix Int
updateN r c m
  | r-1 < 1 = m
  | otherwise = M.setElem 1 (r-1, c) m

updateE :: Int -> Int -> M.Matrix Int -> M.Matrix Int
updateE r c m
  | c+1 > M.ncols m = m
  | otherwise = M.setElem 1 (r, c+1) m

updateS :: Int -> Int -> M.Matrix Int -> M.Matrix Int
updateS r c m
  | r+1 > M.nrows m = m
  | otherwise = M.setElem 1 (r+1, c) m

updateW :: Int -> Int -> M.Matrix Int -> M.Matrix Int
updateW r c m
  | c-1 < 1 = m
  | otherwise = M.setElem 1 (r, c-1) m

-- reduce the transform functions to the updated matrix
updateNeighbors :: Int -> Int -> M.Matrix Int -> M.Matrix Int
updateNeighbors r c m = foldr (\f m -> f r c m) m [updateW, updateS, updateN, updateE]

-- This will set the coordinates for all 1s
-- [[1,0],             [[(1,1),(-1,-1)],
--  [0,0]] will become  [(-1,1),(-1-1)]  will become [(1,1)]
findOnes :: [[Int]] -> [(Int,Int)]
findOnes = pickOnes . concat . M.toLists . M.mapPos setOnes . M.fromLists
    where setOnes (r,c) e = if e == 1 then (r,c) else (-1,-1)
          pickOnes = filter (\x -> x /= (-1,-1))

convert :: [[Int]] -> [[Int]]
convert matrix = M.toLists $ foldl (\m (r, c) -> updateNeighbors r c m) (M.fromLists matrix) (findOnes matrix)

spec :: Spec
spec =
    describe "Matrix conversion" $ do
        it "creates a Matrix" $ do
            -- learning about matrices first
            let input = M.fromLists [[1,0],[0,0]]
                convertedList = [[1,0],[1,0]]

            M.nrows input `shouldBe` 2
            M.getElem 1 1 input `shouldBe` 1
            M.toLists (M.setElem 1 (2,1) input)
                `shouldBe` convertedList
        it "won't change a list that has no 1s" $ do
            let input = [[0,0],[0,0]]
            convert input `shouldBe` input
        it "can convert a list of list to" $ do
            let input = [[1,0],[0,0]]
                result = [[1,1],[1,0]]
            convert input `shouldBe` result
        it "can convert a longer list" $ do
            let result = [[1,1,0,1,1]
                         ,[1,1,1,1,1]
                         ,[1,0,0,1,0]]
            convert matrix `shouldBe` result
