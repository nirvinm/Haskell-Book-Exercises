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


-- Write a Functor instance for a datatype identical to Maybe. We’ll use
-- our own datatype because Maybe already has a Functor instance and
-- we cannot make a duplicate one.
data Possibly a =
      LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- 1. Write a Functor instance for a datatype identical to Either. We’ll
--    use our own datatype because Either has a Functor instance.
data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)
