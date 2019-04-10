module MonadIO where

import Control.Monad.IO.Class
import Control.Monad.Trans
import IdentityT
import EitherT
import MaybeT
import ReaderT
import StateT
import MonadTrans

-- Example MonadIO instances
-- 1. IdentityT
instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

-- 2. EitherT
instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO = lift . liftIO


-- Exercises: Some Instances
-- 1. MaybeT
instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO


-- 2. ReaderT
instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

-- 3. StateT
instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO
