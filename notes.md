## Functors

This is the typeclass for Functors:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

And for Maybe types:

```haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
```

Check out these examples:

```shell
λ> fmap (*2) (Just 4)
Just 8
λ> (*2) <$> (Just 4)
Just 8
λ> (*3) <$> [1..4]
[3,6,9,12]
λ>
```

## Applicatives

This is the typeclass for Applicative Functors. Notice that a Applicative has to be a Functor as well:

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

This gives us functions wrapped in a Just:

```shell
λ> let f = fmap (*) (Just 2)
λ> fmap (\x -> x 3) f
Just 6
```

This gives us functions wrapped in a list:

```shell
λ> let fs = fmap (*) [1,2,3,4]
λ> fmap (\x -> x 3) fs
[3,6,9,12]
λ>
```

The instance implementation for lists:
```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

There is a shorter way to do this:

```shell
λ> (*) <$> Just 2 <*> Just 3
Just 6
```

The instance implementation for Maybe values:
```haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
```

The `<$>` is an abbreviation for fmap.

Same works as well:

```shell
λ> (+) <$> pure 2 <*> [1,2,3,4]
[3,4,5,6]
```

## Type vs Data Constructors
In a data declaration, a type constructor is the thing on the left hand side of the equals sign. The data constructor(s) are the things on the right hand side of the equals sign. You use type constructors where a type is expected, and you use data constructors where a value is expected.

### Data constructors
To make things simple, we can start with an example of a type that represents a colour.

```haskell
data Colour = Red | Green | Blue
```

Here, we have three data constructors. Colour is a type, and Green is a constructor that contains a value of type Colour. Similarly, Red and Blue are both constructors that construct values of type Colour. We could imagine spicing it up though!

```haskell
data Colour = RGB Int Int Int
```

We still have just the type Colour, but RGB is not a value – it’s a function taking three ints and returning a value! RGB has the type.

```haskell
RGB :: Int -> Int -> Int -> Colour
```

RGB is a data constructor that is a function taking some values as its arguments, and then uses those to construct a new value. If you have done any object-oriented programming, you should recognise this. In OOP, constructors also take some values as arguments and return a new value!

In this case, if we apply RGB to three values, we get a colour value!

```shell
Prelude> RGB 12 92 27
#0c5c1b
```

We have constructed a value of type Colour by applying the data constructor. A data constructor either contains a value like a variable would, or takes other values as its argument and creates a new value. If you have done previous programming, this concept shouldn’t be very strange to you.

### Intermission

If you’d want to construct a binary tree to store Strings, you could imagine doing something like:

```haskell
data SBTree = Leaf String
            | Branch String SBTree SBTree
```
What we see here is a type SBTree that contains two data constructors. In other words, there are two functions (namely Leaf and Branch) that will construct values of the SBTree type. If you’re not familiar with how binary trees work, just hang in there. You don’t actually need to know how binary trees work, only that this one stores Strings in some way.

We also see that both data constructors take a String argument – this is the String they are going to store in the tree.

But! What if we also wanted to be able to store Bool, we’d have to create a new binary tree. It could look something like this:

```haskell
data BBTree = Leaf Bool
            | Branch Bool BBTree BBTree
```

### Type constructors

Both SBTree and BBTree are type constructors. But there’s a glaring problem. Do you see how similar they are? That’s a sign that you really want a parameter somewhere.

So we can do this:

```
data BTree a = Leaf a
             | Branch a (BTree a) (BTree a)
```

Now we introduce a type variable a as a parameter to the type constructor. In this declaration, BTree has become a function. It takes a type as its argument and it returns a new type.

It is important here to consider the difference between a concrete type (examples include Int, [Char] and Maybe Bool) which is a type that can be assigned to a value in your program, and a type constructor function which you need to feed a type to be able to be assigned to a value. A value can never be of type “list”, because it needs to be a “list of something”. In the same spirit, a value can never be of type “binary tree”, because it needs to be a "binary tree storing something".

If we pass in, say, Bool as an argument to BTree, it returns the type BTree Bool, which is a binary tree that stores Bools. Replace every occurrence of the type variable a with the type Bool, and you can see for yourself how it’s true.

If you want to, you can view BTree as a function with the kind:

```shell
BTree :: * -> *
```

Kinds are somewhat like types – the * indicates a concrete type, so we say BTree is from a concrete type to a concrete type.

### Wrapping up

Step back here a moment and take note of the similarities.

A data constructor is a “function” that takes 0 or more values and gives you back a new value.<br>
A type constructor is a “function” that takes 0 or more types and gives you back a new type.<br>
Data constructors with parameters are cool if we want slight variations in our values – we put those variations in parameters and let the guy who creates the value decide what arguments they are going to put in. In the same sense, type constructors with parameters are cool if we want slight variations in our types! We put those variations as parameters and let the guy who creates the type decide what arguments they are going to put in.

### A case study

As the home stretch here, we can consider the <code>Maybe a</code> type. It’s definition is:

```haskell
data Maybe a = Nothing
             | Just a
```

Here, Maybe is a type constructor that returns a concrete type. Just is a data constructor that returns a value. Nothing is a data constructor that contains a value. If we look at the type of Just, we see that:

```shell
Just :: a -> Maybe a
```

In other words, Just takes a value of type a and returns a value of type Maybe a. If we look at the kind of Maybe, we see that:

```shell
Maybe :: * -> *
```

In other words, Maybe takes a concrete type and returns a concrete type.
Once again! The difference between a concrete type and a type constructor function. You can not create a list of Maybes. If you try to execute:


```shell
[] :: [Maybe]
```

you’ll get an error. You can however create a list of Maybe Int, or Maybe a. That’s because Maybe is a type constructor function, but a list needs to contain values of a concrete type. Maybe Int and Maybe a are concrete types (or if you want, calls to type constructor functions that return concrete types.)
