{-# LANGUAGE TemplateHaskell #-}

module Katas.Lens.IntroSpec (spec) where

import Test.Hspec
import Control.Lens hiding (element)

data Atom = Atom { _element :: String, _point :: Point } deriving (Show, Eq)
data Point = Point { _x :: Double, _y :: Double } deriving (Show, Eq)
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show, Eq)

shiftAtomX' :: Atom -> Atom
shiftAtomX' (Atom e (Point x y)) = Atom e (Point (x+1) y)

makeLenses ''Atom
makeLenses ''Point
makeLenses ''Molecule

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+1)

{-
    What is lens? A first class getter and setter.
    The actual definition of lens is:
    type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

    How do I create lenses?
    Auto-generate them using Template Haskell or create them by hand.

    Function composition with lenses:

    point :: Lens' Atom Point
    x     :: Lens' Point Double

    point . x :: Lens' Atom Double

    This composite lens lets us get or set the x coordinate of an Atom.
    We can use over and view on the composite Lens' and they will behave as we expect:

    view (point . x) :: Atom -> Double
    over (point . x) :: (Double -> Double) -> (Atom -> Atom)

    Consuming lenses:

    view :: Lens' a b -> a -> b
    over :: Lens' a b -> (b -> b) -> a -> a
    set  :: Lens' a b ->       b  -> a -> a
    set lens b = over lens (\_ -> b)
-}

anAtom :: Atom
anAtom = Atom "c" (Point 1 2)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lens" $ do
        it "is possible accessing nested fields - but awkward" $ do
            shiftAtomX' anAtom `shouldBe` (Atom "c" (Point 2 2))
        it "can shift the x coordinate with Lens" $ do
            let atom = Atom { _element = "c", _point = Point { _x = 1.0, _y = 2.0 }}
            shiftAtomX atom `shouldBe` (Atom "c" (Point 2 2))
        it "works with another data structure" $ do
            let atom2 = Atom "o" (Point 3 4)
            let molecule = Molecule [anAtom, atom2]
            shiftMoleculeX molecule `shouldBe`
                Molecule [(Atom "c" (Point 2 2)), (Atom "o" (Point 4 4))]
        it "can also be used to retrieve values" $ do
            view (point . x) anAtom `shouldBe` 1
        it "has an accessor notation" $ do
            -- it looks like this anAtom ^. (point . x)
            -- (^.) is just an infix operator, equivalent to view
            -- (^.) :: a -> Lens' a b -> b
            -- x ^. l = view l x
            anAtom^.point.x `shouldBe` 1
        it "is First-Class, manipulate them with FP functions" $ do
            let atom2 = Atom "o" (Point 3 4)
            let molecule = Molecule [anAtom, atom2]

            let shift lens = over lens (+1)
            let atomX = point . x
            let moleculeX = atoms . traverse . point . x

            -- note how the composed function is injected into lens
            shift atomX anAtom `shouldBe` (Atom "c" (Point 2 2))
            shift moleculeX molecule
                `shouldBe`
                    Molecule [(Atom "c" (Point 2 2)), (Atom "o" (Point 4 4))]

