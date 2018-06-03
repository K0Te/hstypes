module Queue(benchQueue) where

import Criterion.Main

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a = Queue { enqueue :: [a] , dequeue :: [a] } deriving (Eq, Show)
-- adds an item
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue en de) = Queue (x:en) de

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue en de) = case de of (x:xs) -> Just $ (x, Queue en xs)
                               otherwise -> case en of [] -> Nothing
                                                       otherwise -> pop (Queue [] (reverse en))

xs :: Int -> Maybe (Int, Queue Int)
xs = \n -> pop $ push n $ push 12 $ push 1 $ push 1 empty

pushl :: a -> [a] -> [a]
pushl = (:)

popl :: [a] -> Maybe (a, [a])
popl [] = Nothing
popl l = let ll = length l in Just (l !! (ll-1), take (ll-1) l)

lxs :: Int -> Maybe (Int, [ Int])
lxs = \n -> popl $ pushl n $ pushl 12 $ pushl 1 $ pushl 1 []

benchQueue :: IO ()
benchQueue = defaultMain
  [ bench "queue" $ whnf xs 123
  , bench "list" $ nf lxs 123
  ]