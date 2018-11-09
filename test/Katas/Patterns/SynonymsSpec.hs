{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Katas.Patterns.SynonymsSpec (spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

data Date = Date { month :: Int, day :: Int }
  deriving (Show, Eq)

-- Months
pattern January day  = Date { month = 1, day  = day }
pattern February day = Date { month = 2, day  = day }
pattern March day    = Date { month = 3, day  = day }
-- elided
pattern December day = Date { month = 12, day = day }

-- Holidays
pattern Christmas = Date { month = 12, day = 25 }
pattern Christmas' = December 25

-- Uni-directional patterns
-- show
pattern BeforeChristmas {-hi-}<-{-/hi-} December (compare 25 -> GT)
pattern Xmas            {-hi-}<-{-/hi-} December (compare 25 -> EQ)
pattern AfterChristmas  {-hi-}<-{-/hi-} December (compare 25 -> LT)

describe' :: Date -> String
describe' (January 1)  = "First day of year"
describe' (February n) = show n ++ "th of February"
describe' Christmas    = "Presents!"
describe' Christmas'   = "Presents!!"
describe' _            = "meh"

react :: Date -> String
react BeforeChristmas = "Waiting :()"
react Christmas       = "Presents!"
react AfterChristmas  = "Have to wait a whole year :()"
react _               = "It's not even December..."

-- This is equivalent to:
react' (Date 12 (compare 25 -> GT)) = "Waiting :("
react' (Date 12 (compare 25 -> EQ)) = "Presents!"
react' (Date 12 (compare 25 -> LT)) = "Have to wait a whole year :("
react' _                            = "It's not even December..."

-- Accessing values
days'tilChristmas :: Date -> Int
days'tilChristmas d@BeforeChristmas = 25 - day d
days'tilChristmas Xmas              = 0
days'tilChristmas d@AfterChristmas  = 365 + 25 - day d

-- But a nicer way to write it
isItNow :: Int -> (Ordering, Int)
isItNow day = (compare 25 day, day)

pattern BeforeChristmas' day <- December (isItNow -> (GT, day))
pattern Xmas'                <- December (isItNow -> (EQ, _))
pattern AfterChristmas' day  <- December (isItNow -> (LT, day))

days'tilChristmas' :: Date -> Int
days'tilChristmas' (BeforeChristmas' n) = 25 - n
days'tilChristmas' Xmas'                = 0
days'tilChristmas' (AfterChristmas' n)  = 365 + 25 - n

spec :: Spec
spec = do
  describe "Synonyms - Bidirectional Patterns" $ do
    it "matches by patterns" $ do
      describe' (Date 1 1) `shouldBe` "First day of year"
      describe' (Date 12 25) `shouldBe` "Presents!"
      describe' (Date 2 5) `shouldBe` "5th of February"
    it "can be used as expressions as well" $ do
      describe' Christmas `shouldBe` "Presents!"
      describe' (February 10) `shouldBe` "10th of February"
      March 5 `shouldBe` Date { month = 3, day = 5 }
    it "can build pattern from one pattern" $
     describe' Christmas' `shouldBe` "Presents!"
  describe "Synonyms - Unidirectional Patterns" $
    it "can pattern match more complex expressions" $ do
      react (Date 12 24) `shouldBe` "Waiting :()"
      react (Date 12 25) `shouldBe` "Presents!"
      react (Date 12 26) `shouldBe` "Have to wait a whole year :()"
      react (Date 11 31) `shouldBe` "It's not even December..."
      react' (Date 12 25) `shouldBe` "Presents!"
  describe "Accessing Values" $ do
    it "can access matched values" $ do
      days'tilChristmas (Date 12 24) `shouldBe` 1
      days'tilChristmas (Date 12 25) `shouldBe` 0
      days'tilChristmas (Date 12 26) `shouldBe` 364
    it "works with a nicer expression" $ do
      days'tilChristmas' (Date 12 24) `shouldBe` 1
      days'tilChristmas' (Date 12 25) `shouldBe` 0
      days'tilChristmas (Date 12 27) `shouldBe` 363

