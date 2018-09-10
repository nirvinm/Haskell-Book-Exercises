module ChapterExercises where

import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write Monad instances for the following types. Use the QuickCheck
-- properties we showed you to validate your instances.

-- 1. Welcome to the Nope Monad, where nothing happens and nobody cares.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ =  NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    _ >>= _ = NopeDotJpg

instance (Eq a) => EqProp (Nope a) where
    (=-=) = eq

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

--  2.
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a) = Left' $ f a

instance Applicative (PhhhbbtttEither b) where
    pure x = Left' x
    (Right' x)  <*> _ = Right' x
    _ <*> (Right' x) = Right' x
    (Left' f) <*> (Left' x) = Left' $ f x
    
instance Monad (PhhhbbtttEither b) where
    return = pure
    (Right' x) >>= _ = Right' x
    (Left' x) >>= f = f x
    
instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = oneof [ Left' <$> arbitrary,
                        Right' <$> arbitrary ]


-- 3. Write a Monad instance for Identity.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x 

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

-- 4. This one should be easier than the Applicative instance wasRemember to use the Functor that Monad requires, then see where the chips fall.
data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
    ls <> Nil = ls
    Nil <> ls = ls
    (Cons x xs) <> ys = Cons x (xs <> ys)

instance  Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x ls) = Cons (f x) (fmap f ls)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f ls) <*> xs = (fmap f xs) `mappend` (ls <*> xs)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (Cons x xs) >>= f = f x <> (xs >>= f)

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = oneof [ return Nil,
                        Cons <$> arbitrary <*> arbitrary ]


--Write the following functions using the methods provided by
--Monad and Functor. Using stuff like identity and composition --is fine,
--but it has to typecheck with types provided.
-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- 5. You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (:) (f x) (meh xs f)

-- 6. Hint: reuse “meh”
flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
