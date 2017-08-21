-- Write the function myIterate using direct recursion. Compare
-- the behavior with the built-in iterate to gauge correctness. Do
-- not look at the source or any examples of iterate so that you
-- are forced to do this yourself.
myIterate :: (a -> a) -> a -> [a]
myIterate f seed = seed:(myIterate f $ f seed)


-- Write the function myUnfoldr using direct recursion. Compare
-- with the built-in unfoldr to check your implementation. Again,
-- don’t look at implementations of unfoldr so that you figure it
-- out yourself.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f seed = case f seed of 
                    Nothing -> []
                    Just (a,b) -> a:(myUnfoldr f b)


betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\k -> Just (k, f k))

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)


-- Write unfold for BinaryTree.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f seed = case f seed of
                    Just (l,m,r) -> Node (unfold f l) m (unfold f r)
                    Nothing -> Leaf


-- Make a tree builder.
-- Using the unfold function you’ve made for BinaryTree, write the
-- following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild n
    | n < 1     = Leaf
    | otherwise = unfold f 0
                    where f k
                            | k == n    = Nothing
                            | otherwise = Just (k+1,k,k+1)
