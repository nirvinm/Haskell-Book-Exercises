{-# LANGUAGE InstanceSigs #-}

module Compose where

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga)
        = Compose $ (fmap . fmap) f fga


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose <$> pure.pure

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose f) <*> (Compose a)
        = Compose $ (<*>) <$> f <*> a

{-
instance (Monad f, Monad g) => Monad (Compose f g) where  
    (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
    (Compose fga) >>= h = (fmap.fmap) h fga
-}

-- Exercises: Compose Instances
-- 1. Write the Compose Foldable instance.
-- The foldMap = undefined bit is a hint to make it easier and look 
-- more like what you’ve seen already.
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m
            => (a -> m)
            -> Compose f g a
            -> m
    foldMap h (Compose fga)
        = foldMap (foldMap h) fga


-- 2. Write the Compose Traversable instance.
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative f1 
             => (a -> f1 b)
             -> Compose f g a
             -> f1 (Compose f g b)
    traverse h (Compose fga)
        = Compose <$> traverse (traverse h) fga

