module Lib where

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity  where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

-- Can you implement one for this type? Why? Why not?
--   Since the kind of this type is * and not * -> *,
--   a Functor instance cannot be defined.
data Trivial = Trivial
