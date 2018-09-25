module Lib where

import Control.Monad

-- Write bind in terms of fmap and join.
-- Fear is the mind-killer, friend. You can do it.
-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join.fmap f
