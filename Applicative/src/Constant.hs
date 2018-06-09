module Constant where

-- Exercise: Constant Instance
-- Write an Applicative instance for Constant.
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant f) <*> (Constant a) = Constant a
