module FixUpper where

-- Exercise: Fixer Upper
-- Given the function and values provided, use (<$>) from Functor, (<*>)
-- and pure from the Applicative typeclass to fill in missing bits of the
-- broken code to make it work.
-- 1.
f :: Maybe String
f = const <$> Just "Hello" <*> pure "World"

-- 2.
g :: Maybe (Int, Int, String, [Int])
g = (,,,) <$> Just 90
          <*> Just 10
          <*> Just "Tierness"
          <*> pure [1, 2, 3]

