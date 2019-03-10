module IdentityT where

newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT $ f <$> fa


instance Applicative Identity where
    pure = Identity
    (Identity fa) <*> (Identity a) = Identity $ fa a

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT $ pure x
    (IdentityT fa) <*> (IdentityT a) = IdentityT $ fa <*> a


instance Monad Identity where
    (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (IdentityT ma) >>= f = IdentityT (ma >>= runIdentityT.f)

