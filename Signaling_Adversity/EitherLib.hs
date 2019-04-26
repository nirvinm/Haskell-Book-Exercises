-- Try to eventually arrive at a solution that uses foldr, even if
-- earlier versions don’t use foldr.

lefts' :: [Either a b] -> [a]
lefts' ls = foldr f [] ls
            where f k acc = case k of
                                (Left l) -> l:acc
                                _ -> acc

rights' :: [Either a b] -> [b]
rights' ls = foldr f [] ls
             where f k acc = case k of
                                (Right r) -> r:acc
                                _ -> acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' ls = (lefts' ls, rights' ls)


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right r) = Just $ f r
eitherMaybe' _ _ = Nothing

-- This is a general catamorphism for Either values.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ f (Right r) = f r

-- Same as before, but use the either' function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f k = Just $ either' undefined f k
