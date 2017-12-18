module Lib where

newtype Identity a = Identity a

instance Functor Identity  where
    fmap f (Identity a) = Identity (f a)

