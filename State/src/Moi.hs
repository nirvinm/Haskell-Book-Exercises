{-# Language InstanceSigs #-}

module Moi where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Write State for yourself
newtype Moi s a = Moi { runMoi :: s -> (a, s) } deriving (Show)

-- State Functor
-- Implement the Functor instance for State.
instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s0 ->
        let (a, s1) = g s0
        in (f a, s1)

-- State Applicative
-- Write the Applicative instance for State.
instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) =  Moi $ \s0 ->
        let (fa, s1) = f s0
            (a, s2) = g s1
        in (fa a, s2)

-- State Monad
-- Write the Monad instance for State.
instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s0 ->
        let (a, s1) = f s0
        in runMoi (g a) s1


instance (Arbitrary s, Arbitrary a) => Arbitrary (Moi s a) where
    arbitrary = do
        a <- arbitrary
        s <- arbitrary
        return $ Moi $ \s0 -> (a, s)

instance (Num s, Eq s, Eq a) => EqProp (Moi s a) where
   (Moi x) =-= (Moi y) = (x 0) `eq` (y 0)
