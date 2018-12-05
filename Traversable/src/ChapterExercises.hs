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


-- Maybe
data Optional a =
    Nada
  | Yep a

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure $ Nada
    traverse f (Yep a) = Yep <$> f a


-- List
data List a =
    Nil
  | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    
instance (Semigroup a) => Semigroup (List a) where
    (Cons x xs) <> ys = Cons x $ xs <> ys

instance (Monoid a) => Monoid (List a) where
    mempty = Nil
    mappend xs ys = xs <> ys

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
    sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs


