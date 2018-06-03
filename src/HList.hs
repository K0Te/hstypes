module HList(doBench) where

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL $ \l -> l
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL $ \l -> a:l
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList x = unDL x []
{-# INLINE toList #-}

-- Prepend a single element to a dlist. infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc dl a = DL $ \l -> (unDL dl l) ++ [a]

-- Append dlists.
append :: DList a -> DList a -> DList a
append dl1 dl2 = DL $ \l -> unDL dl2 (unDL dl1 l)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)
constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

doBench :: IO ()
doBench = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]