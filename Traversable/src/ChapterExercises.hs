{-# LANGUAGE FlexibleContexts #-}

module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- 21.12 Chapter Exercises

-- Traversable instances

-- Write a Traversable instance for the datatype provided, filling in any
-- required superclasses. Use QuickCheck to validate your instances.

-- Identity
-- Write a Traversable instance for Identity

newtype Identity a = Identity a
                        deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
    foldMap f (Identity x) = f x

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq


-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap f (Constant x) = mempty

instance Traversable (Constant a) where
    traverse f (Constant x) = pure $ Constant x

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq a, Eq b) => EqProp (Constant a b) where
    (=-=) = eq

-- Maybe
data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure $ Nada
    traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = oneof [ Yep <$> arbitrary,
                        pure Nada ]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq



-- List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)
    
instance (Semigroup a) => Semigroup (List a) where
    xs <> Nil = xs
    Nil <> ys = ys
    (Cons x xs) <> ys = Cons x $ xs <> ys

instance (Monoid a) => Monoid (List a) where
    mempty = Nil
    
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
    sequenceA Nil = pure Nil
    sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = oneof [ Cons <$> arbitrary <*> arbitrary,
                        pure Nil ]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq
    

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = undefined
    
instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y $ f z

instance Foldable (Three a b) where
    foldMap f (Three x y z) = f z

instance Traversable (Three a b) where
    sequenceA (Three x y z) = (Three x y) <$> z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    (Pair x y) <> (Pair x' y') = Pair (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair mempty mempty

instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x $ f y

instance Foldable (Pair a) where
    foldMap f (Pair x y) = f y

instance Traversable (Pair a) where
    sequenceA (Pair x y) = (Pair x) <$> y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq


-- Big
-- When you have more than one value of type b, you’ll want to use
-- Monoid and Applicative for the Foldable and Traversable instances re-
-- spectively.
data Big a b = Big a b b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Big a b) where
    (Big x y z) <> (Big x' y' z') = Big (x <> x') (y <> y') (z <> z')

instance (Monoid a, Monoid b) => Monoid (Big a b) where
    mempty = Big mempty mempty mempty

instance Functor (Big a) where
    fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
    foldMap f (Big x y z) = (f y) <> (f z)

instance Traversable (Big a) where
    sequenceA (Big x y z) = (Big x) <$> y <*> z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq


-- Instances for Tree
-- This might be hard. Write the following instances for Tree.
data Tree a =
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)
    
instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

instance Traversable Tree where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node left a right)  = Node <$> (traverse f left) <*> (f a) <*> (traverse f right)

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [ pure Empty,
                        Leaf <$> arbitrary,
                        Node <$> arbitrary <*> arbitrary <*> arbitrary ]

instance (Eq a) => EqProp (Tree a) where
    (=-=) = eq
