module SkewBinaryRandomAccessList where

-- Random access list, represneted as list of trees -> similar to skew binary numbers
-- Allows O(1) head, tail, cons
-- O(log(n)) random element lookup and update
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
newtype RList a = RList [(Int, Tree a)] deriving Show
-- Int - weight of corresponding tree
-- Elements should be retreieved in following order - root, left, right

empty :: RList a
empty = RList []

isEmpty :: RList a -> Bool
isEmpty (RList lst) = null lst

cons :: a -> RList a -> RList a
cons x (RList rst@((s1, t1):(s2, t2):xs)) = RList $ if s1 == s2
  then (s1+s2+1, Node x t1 t2):xs
  else (1, Leaf x):rst
cons x (RList rst) = RList $ (1, Leaf x):rst

head :: RList a -> a
head (RList []) = error "Empty list"
head (RList ((_, Leaf x):_)) = x
head (RList ((_, Node x _ _):_)) = x

tail :: RList a -> RList a
tail (RList []) = error "Empty list"
tail (RList ((_, Leaf _):rst)) = RList rst
tail (RList ((w, Node _ left right):rst)) = RList $ (w `div` 2, left):(w `div` 2, right):rst

-- Find element in tree by index
lookupInTree :: Int -> (Int, Tree a) -> a
lookupInTree 0 (_, Leaf x) = x
lookupInTree _ (_, Leaf _) = error "Index too large"
lookupInTree 0 (_, Node x _ _) = x
lookupInTree n (w, Node _ left right) = let mid = w `div` 2 in
  if n > mid
  then lookupInTree (n-mid-1) (mid, right)
  else lookupInTree (n-1) (mid, left)

lookupRList :: Int -> RList a -> a
lookupRList _ (RList []) = error "Empty"
lookupRList n (RList ((weight, tree):trees)) = if n<=weight-1
  then lookupInTree n (weight, tree)
  else lookupRList (n-weight) (RList trees)

updateInTree :: Int -> a -> (Int, Tree a) -> (Int, Tree a)
updateInTree 0 value (weight, Leaf _) = (weight, Leaf value)
updateInTree _ _ (_, Leaf _) = error "Index too large"
updateInTree 0 value (weight, Node _ left right) = (weight, Node value left right)
updateInTree n value (weight, Node x left right) = let mid = weight `div` 2 in
  if n > mid
  then (weight, Node x left (snd $ updateInTree (n-1-mid) value (mid, right)))
  else (weight, Node x (snd $ updateInTree (n-1) value (mid, left)) right)

unpack :: RList a -> [(Int, Tree a)]
unpack (RList x) = x

updateRList :: Int -> a -> RList a -> RList a
updateRList _ _ (RList []) = error "Empty"
updateRList n x (RList ((weight, tree):trees)) = if n<=weight-1
  then RList $ updateInTree n x (weight, tree) : trees
  else RList $ (weight, tree) : (unpack $ updateRList (n-weight) x (RList trees))
