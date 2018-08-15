module Katas.Exercises.MapsAndSetsSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Monoid
import Text.Printf

main :: IO ()
main = hspec spec

-- ideas from here: https://haskell-lang.org/library/containers

data SSN = SSN
    { ssnPrefix :: Int
    , ssnInfix :: Int
    , ssnSuffix :: Int
    } deriving (Eq, Ord)

instance Show SSN where
    show (SSN p i s) = printf "%03d-%02d-%04d" p i s

data Gender = Male | Female deriving (Eq, Show)

data Person = Person
    { firstName :: String
    , lastName :: String
    , gender :: Gender
    } deriving (Eq)

instance Show Person where
    show (Person fName lName g) = fName ++ ' ':lName ++ " (" ++ show g ++ ")"

type Employees = Map.Map SSN Person

mkSSN :: Int -> Int -> Int -> SSN
mkSSN p i s
    | p <= 0 || p == 666 || p >= 900 = error $ "Invalid SSN prefix: " ++ show p
    | i <= 0 || i > 99 = error $ "Invalid SSH infix: " ++ show i
    | s <= 0 || s > 9999 = error $ "Invalid SSN suffix: " ++ show s
    | otherwise = SSN p i s

-- create employees
employees :: Employees
employees =
    Map.fromList
        [ (mkSSN 525 21 5423, Person "John" "Doe" Male)
        , (mkSSN 521 01 8756, Person "Mary" "Jones" Female)
        , (mkSSN 585 11 1234, Person "William" "Smith" Male)
        , (mkSSN 525 15 5673, Person "Maria" "Gonzalez" Female)
        , (mkSSN 524 34 1234, Person "Bob" "Jones" Male)
        , (mkSSN 522 43 9862, Person "John" "Doe" Male)
        , (mkSSN 527 75 1035, Person "Julia" "Bloom" Female)
        ]

lookupEmployee :: SSN -> Employees -> Maybe Person
lookupEmployee = Map.lookup

showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = List.intercalate "\n" . map show . Map.toList

showEmployee :: (SSN, Person) -> String
showEmployee (social, person) =
    concat [show social, ": ", show person]

showEmployees :: Employees -> String
showEmployees es
    | Map.null es = ""
    | otherwise = showE ssn0 person0 ++ Map.foldrWithKey prepender "" rest
    where
        showE = curry showEmployee
        ((ssn0, person0), rest) = Map.deleteFindMin es
        prepender key person acc = '\n' : showE key person ++ acc

employeeNames :: Employees -> String
employeeNames es =
    intercalate " - " $
    fmap (\p -> firstName p ++ " " ++ lastName p) $
    Map.elems es

spec :: Spec
spec = describe "Maps and Sets" $ do
    it "look up employee" $ do
        lookupEmployee (mkSSN 524 34 1234) employees
            `shouldBe` Just (Person "Bob" "Jones" Male)
        lookupEmployee (mkSSN 555 12 3456) employees
            `shouldBe` Nothing
    it "checks presence by SSN" $ do
        mkSSN 585 11 1234 `Map.member` employees
            `shouldBe` True
        mkSSN 621 24 8736 `Map.member` employees
            `shouldBe` False
    it "can look up employee with a default Person" $ do
        Map.findWithDefault
            (Person "Bill" "Smith" Male)
            (mkSSN 585 11 1234)
            employees
            `shouldBe` Person "William" "Smith" Male
        Map.findWithDefault
            (Person "Anthony" "Richardson" Male)
            (mkSSN 621 24 8736)
            employees
            `shouldBe` Person "Anthony" "Richardson" Male
    it "can report the size" $ do
        Map.size (Map.delete (mkSSN 585 11 1234) employees)
            `shouldBe` 6
        Map.size (Map.delete (mkSSN 621 24 8736) employees)
            `shouldBe` 7
    it "can add a new employee" $
        Map.size (Map.insert (mkSSN 621 24 8736)
                             (Person "Anthony" "Richardson" Male)
                             employees)
            `shouldBe` 8
    it "can show the employees in raw form" $
        showMap employees `shouldStartWith`
            "(521-01-8756,Mary Jones (Female))"
    it "can be displayed in a better format" $
        showEmployees employees
            `shouldStartWith`
            "521-01-8756: Mary Jones (Female)"
    it "can map over its values" $
        Map.elems (Map.map lastName employees)
            `shouldBe` ["Jones","Doe","Jones","Gonzalez","Doe","Bloom","Smith"]
    it "can map over its keys" $
        Map.keys (Map.mapKeys (show . ssnPrefix) employees)
            `shouldBe` ["521","522","524","525","527","585"]
    it "can filter its values" $
        employeeNames (Map.filter (("Jones"==) . lastName) employees)
            `shouldBe` "Mary Jones - Bob Jones"
    it "can partition map values" $ do
        let (men, women) = Map.partition ((Male==) . gender) employees
        employeeNames women
            `shouldBe` "Mary Jones - Maria Gonzalez - Julia Bloom"
