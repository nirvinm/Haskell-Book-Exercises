module EitherT where

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

-- Exercises: EitherT
-- 1. Write the Functor instance for EitherT:
instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mmea) = EitherT $ (fmap.fmap) f mmea


-- 2. Write the Applicative instance for EitherT:
instance Applicative m => Applicative (EitherT e m) where
    pure a = EitherT $ pure (pure a)
    (EitherT mmefa) <*> (EitherT mmea) =
        EitherT $ (<*>) <$> mmefa <*> mmea


-- 3. Write the Monad instance for EitherT:
instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT mmea) >>= f = EitherT $ do
        mea <- mmea
        case mea of
            Left e -> return $ Left e
            Right a -> runEitherT (f a)


-- 4. Write the swapEitherT helper function for EitherT.
-- transformer version of swapEither.
-- Hint: write swapEither first, then swapEitherT in terms of the former.
swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) =  Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mmea) = EitherT $ swapEither <$> mmea


--5. Write the transformer variant of the either catamorphism.
eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mmab) = mmab >>= either f g
