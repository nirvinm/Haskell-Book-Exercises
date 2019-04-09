module MonadTrans where

import Control.Monad.Trans
import IdentityT
import MaybeT
import ReaderT
import EitherT
import StateT

-- 1. IdentityT
instance MonadTrans IdentityT where
    lift = IdentityT

-- 2. MaybeT
instance MonadTrans MaybeT where
    lift = MaybeT . (fmap Just)

-- 3. ReaderT
instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

-- Exercises: Lift More
-- Keep in mind what these are doing, follow the types, lift till you drop.

-- 1. You thought you were done with EitherT.
instance MonadTrans (EitherT e) where
    lift = EitherT . (fmap Right)

-- 2. Or StateT. This one’ll be more obnoxious. It’s fine if you’ve seen
-- this before.
instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do
        a <- ma
        return (a, s)
