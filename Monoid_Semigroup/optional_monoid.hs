-- Write the Monoid instance for our Maybe type renamed to Optional.

-- Expected output:
-- Prelude> Only (Sum 1) `mappend` Only (Sum 1)
-- Only (Sum {getSum = 2})

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where 
    mempty = Nada
    mappend (Only x) (Only y) = Only (x <> y)
    mappend (Only x) _ = Only x
    mappend _ (Only y) = Only y
    mappend _ _ = Nada

