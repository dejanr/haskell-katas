module Katas.Monads.MonadLawsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad

main :: IO ()
main = hspec spec

{-
   Monad laws:

    1. Left Identity

       Take a value, put it in the default context with return
       and then feed to a function by using `>>=`, it's the same
       as just taking the value and applying the function to it.
       return x >>= f is the same as f x

       λ> return 3 >>= (\x -> return (x*2))
       6
       λ> Just 2 >>= (\x -> return (x*2))
       Just 4

       For the list monad, return puts something in a singleton list,
       the >>= implementation for lists goes over all the values
       in the list and applies the function to them.

       λ> [1,2,3] >>= (\x -> [x*3])
       [3,6,9]


    2. Right Identity

       If we have a monadic value and we use >>= to feed it to return,
       then the result is our original monadic value.
       m >>= return is no different than just m

       λ> Just 4 >>= (\x -> return x)
       Just 4

    3. Associativity

       When we have a chain of monadic function application with
       >>=, it shouldn't matter how they're nested.
       (m >>= f) >>= g is just like doing m >>= (\x -> f x >> g)

       λ> Just 2 >>= (\x -> return (x * 2) >>= (\y -> return (y * 3)))
       Just 12

       λ> (Just 2 >>= (\x -> return (x * 2))) >>= (\y -> return (y * 3))
       Just 12
-}

type Birds = Int
type Pole = (Birds,Birds)

-- Improved logic with checks
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

spec :: Spec
spec =
    describe "Monad Laws" $ do
        it "abides by the first law - left identity" $
            (return 3 >>= \x -> Just (x+1000))
                 `shouldBe` (\x -> Just (x+1000)) 3
        it "holds true with lists" $
            (return "WoM" >>= \x -> [x,x,x])
                `shouldBe` (\x -> [x,x,x]) "WoM"
        it "abides by the second law - right identity" $ do
            (return "move on up" >>= return)
                `shouldBe` Just "move on up"
            ([1,2,3,4] >>= return)
                `shouldBe` [1,2,3,4]
        it "abides by the third law - associativity" $ do
            -- It lands 2 on the right, then 2 on the left and again 2 on the right
            (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2)
                `shouldBe` Just (2,4)
            (((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2)
                `shouldBe` Just (2,4)
