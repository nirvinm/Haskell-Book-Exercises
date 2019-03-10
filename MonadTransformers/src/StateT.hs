{-# LANGUAGE InstanceSigs #-}

module StateT where

import Data.Tuple

-- Exercises: StateT
newtype StateT s m a =
    StateT { runStateT :: s -> m (a,s) }


-- 1. You’ll have to do the Functor and Applicative instances first, be-
-- cause there aren’t Functor and Applicative instances ready to go
-- for the type Monad m => s -> m (a, s).
instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT smas) =
        StateT $ (fmap.fmap) go smas
            where go (a, s) = (f a, s)


-- 2. As with Functor, you can’t cheat and reuse an underlying Applicative
-- instance, so you’ll have to do the work with the s -> m (a, s)
-- type yourself.
instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)
    
    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT smfa) <*> (StateT sma) =
        StateT $ \s0 -> do
            (fa, s1) <- smfa s0
            (a, s2) <- sma s1
            return (fa a, s2)

-- 3. The Monad instance should look fairly similar to the Monad instance
-- you wrote for ReaderT.
instance Monad m => Monad (StateT s m) where
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= f =
        StateT $ \s0 -> do
            (a, s1) <- sma s0
            runStateT (f a) s1