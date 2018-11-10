module PurelyFuncBinomHeap where

data Tree a = Node { rank :: Int
                   , root :: a
                   , children :: [Tree a]
                   } deriving Show
newtype Heap a = Heap [Tree a]
unheap :: Heap a -> [Tree a]
unheap (Heap ts) = ts

linkTrees :: Ord a => Tree a -> Tree a -> Tree a
linkTrees t1 t2
  | root t1 <= root t2 = Node (rank t1 + 1) (root t1) (t2:children t1)
  | otherwise = Node (rank t1 + 1) (root t2) (t1:children t2)

insertTree :: Ord a => Tree a -> Heap a -> Heap a
insertTree t (Heap []) = Heap [t]
insertTree t (Heap (x:xs))
  | rank x > rank t = Heap (t:x:xs)
  | otherwise = insertTree (linkTrees t x) (Heap xs)

insertElem :: Ord a => a -> Heap a -> Heap a
insertElem x heap = insertTree (Node 0 x []) heap

mergeHeaps :: Ord a => Heap a -> Heap a -> Heap a
mergeHeaps (Heap []) h2 = h2
mergeHeaps h1 (Heap []) = h1
mergeHeaps (Heap xx@(x:xs)) (Heap yy@(y:ys))
  | rank x < rank y = Heap (x: (unheap (mergeHeaps (Heap xs) (Heap yy))))
  | rank x > rank y = Heap (y: (unheap (mergeHeaps (Heap ys) (Heap xx))))
  | rank x == rank y = insertTree (linkTrees x y) (mergeHeaps (Heap xs) (Heap ys))

findMin :: Ord a => Heap a -> a
findMin (Heap []) = error "Empty"
findMin (Heap [t]) = root t
findMin (Heap (t:ts)) = min (root t) (findMin (Heap ts))

popMin :: Ord a => Heap a -> (a, Heap a)
popMin (Heap []) = error "Empty"
popMin (Heap ts) = let (node, rest_trees) = getMinTree ts in (root node, mergeHeaps (Heap (reverse $ children node)) (Heap rest_trees))
  where getMinTree [t] = (t, [])
        getMinTree (t:ts) = let (min_other, rest) = getMinTree ts in if root t < root min_other then (t, ts) else (min_other, t:rest)

