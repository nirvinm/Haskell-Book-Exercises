module DList where


newtype DList a = DL { unDL :: [a] -> [a] }


-- 1.
empty :: DList a
empty = DL ([] ++)
{-# INLINE empty #-}

-- 2.
singleton :: a -> DList a
singleton x = DL ([x] ++)
{-# INLINE singleton #-}

-- 3.
toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

-- 4. Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- 5. Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc (DL xs) x = DL $ xs.([x]++)
{-# INLINE snoc #-}

-- 6. Append dlists.
append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL $ xs.ys
{-# INLINE append #-}

-- 7. Create a new DList from List.
fromList :: [a] -> DList a
fromList = foldl snoc empty

