module LibraryFunctions where

import Data.Monoid

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Exercises: Library Functions
-- Implement the functions in terms of foldMap or foldr from Foldable,
-- then try them out with multiple types that have Foldable instances.

-- 1. This and the next one are nicer with foldMap, but foldr is fine too.
sum :: (Foldable t, Num a) => t a -> a
sum = getSum.(foldMap Sum)

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = getProduct.(foldMap Product)

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr ((||).(== x)) False

-- 4.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = foldr f Nothing xs where
                f a Nothing = Just a
                f a (Just b) = Just (min a b)

--5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = foldr f Nothing xs where
                f a Nothing = Just a
                f a (Just b) = Just (max a b)

--6.
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

--7.
length :: (Foldable t) => t a -> Int
length = foldr (\_ sum -> sum + 1) 0

--8. Some say this is all Foldable amounts to.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9. Hint: use foldMap.
-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- 10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend.f) mempty

