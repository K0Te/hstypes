module PurelyRandomAccessList where

-- Random access list, which is a binary tree list
data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
data Digit a = Zero | One (Tree a) deriving Show
newtype RList a = RList [Digit a] deriving Show

empty :: RList a
empty = RList []

isEmpty :: RList a -> Bool
isEmpty (RList lst) = null lst

size :: Tree a -> Int
size (Leaf _) = 1
size (Node x _ _) = x

link :: Tree a -> Tree a -> Tree a
-- t1 and t2 should have same size ?
link t1 t2 = Node (size t1 + size t2) t1 t2

unpack :: RList a -> [Digit a]
unpack (RList x) = x

insertTree :: Tree a -> RList a -> RList a
insertTree t (RList []) = RList [One t]
insertTree t (RList (Zero:xs)) = RList ((One t):xs)
insertTree t (RList (One x: xs)) = RList (Zero:(unpack $ insertTree (link t x) (RList xs)))

borrowTree :: RList a -> (Tree a, RList a)
borrowTree (RList []) = error "Empty"
borrowTree (RList ((One t):xs)) = (t, RList (Zero:xs))
borrowTree (RList (Zero:xs)) = let
  (Node _ t1 t2, RList lst2) = borrowTree (RList xs)
  in (t1, RList ((One t2):lst2))

cons :: a -> RList a -> RList a
cons x t = insertTree (Leaf x) t

head :: RList a -> a
head (RList []) = error "Empty list"
head t = let (Leaf x, _) = borrowTree t in x

tail :: RList a -> RList a
tail (RList []) = error "Empty list"
tail t = let (_, rest) = borrowTree t in rest

lookupInTree :: Int -> Tree a -> a
lookupInTree 0 (Leaf x) = x
lookupInTree _ (Leaf _) = error "No such index"
lookupInTree n (Node sz t1 t2) = if n < sz `div` 2 then lookupInTree n t1 else lookupInTree (n - (sz `div` 2)) t2

lookupL :: Int -> RList a -> a
lookupL _ (RList []) = error "Empty list"
lookupL n (RList (Zero:xs)) = lookupL n (RList xs)
lookupL n (RList ((One t):xs)) = if n < size t then lookupInTree n t else lookupL (n-size t) (RList xs)

updateInTree :: (Int, a) -> Tree a -> Tree a
updateInTree (0, v) (Leaf _) = Leaf v
updateInTree _ (Leaf _) = error "Empty tree"
updateInTree (n, val) (Node sz t1 t2) =
  if n < sz `div` 2
  then Node sz (updateInTree (n, val) t1) t2
  else Node sz t1 (updateInTree (n - (sz `div` 2), val) t2)

update :: (Int, a) -> RList a -> RList a
update _ (RList []) = error "Empty list"
update (n, val) (RList (Zero:xs)) = update (n,val) (RList xs)
update (n, val) (RList ((One t):xs)) =
  let t_size = size t in
  if n < t_size
  then RList $ (One $ updateInTree (n, val) t):xs
  else RList $ (One t):(unpack $ update (n-t_size, val) (RList xs))
