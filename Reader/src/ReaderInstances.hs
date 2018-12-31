{-# LANGUAGE InstanceSigs #-}

module ReaderInstances where

newtype Reader r a = Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb  = f <$> fa <*> fb


asks :: (r -> a) -> Reader r a
asks = Reader


instance Functor (Reader r) where
    fmap :: (a -> b) -> (Reader r a) -> (Reader r b)
    fmap f (Reader ra) = Reader $ f.ra

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader f) <*> (Reader ra) = Reader $ \r -> f r (ra r)


instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    g >>= f = Reader $ \r -> runReader (f (runReader g r)) r