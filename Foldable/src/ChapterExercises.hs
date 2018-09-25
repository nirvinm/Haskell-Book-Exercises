module ChapterExercises where

import Data.Monoid

-- Write Foldable instances for the following datatypes.
-- 1.
data Constant a b = Constant b

instance (Semigroup b) => Semigroup (Constant a b) where
    (Constant x) <> (Constant y) = Constant (x <> y)

instance (Monoid b) => Monoid (Constant a b) where
    mempty = Constant mempty

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

-- 2.
data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

-- Skipping similar exercises --



-- Thinking cap time. Write a filter function for Foldable types using foldMap.
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\k -> if f k then pure k else mempty)

