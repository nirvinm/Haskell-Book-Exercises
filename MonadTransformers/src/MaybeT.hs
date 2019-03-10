{-# LANGUAGE InstanceSigs #-}

module MaybeT where

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT mma) = MaybeT $ (fmap.fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where
    pure a = MaybeT $ pure (Just a)
    (MaybeT mmfa) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> mmfa <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT mma) >>= f = MaybeT $ do
        ma <- mma
        case ma of
            Nothing -> return Nothing
            Just a  -> runMaybeT (f a)
