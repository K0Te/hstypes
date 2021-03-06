{-# Language RecordWildCards #-}
{-# Language NamedFieldPuns #-}

module PurelyFunctQueue where

import Criterion.Main
import Control.DeepSeq
import Data.List (foldl')

data Queue a = Queue {
    fF :: [a]
  , lenF :: Int
  , r :: [a]
  , lenR :: Int
  } deriving Show

instance NFData a => NFData (Queue a) where
  rnf (Queue {..}) = rnf fF `seq` rnf r `seq` rnf lenR `seq` rnf lenR

--􏰂 Invariants: length fF >= length  􏰅r
-- lenF = length fF
-- lenR = length r
empty :: Queue a
empty = Queue {fF = [], lenF = 0, r = [], lenR = 0}

isEmpty :: Queue a -> Bool
isEmpty q = lenF q > 0

queue :: Queue a -> Queue a -- sort of Queue smart constructor
queue (q@Queue {..})
  | lenF >= lenR = q
  | otherwise = Queue {fF = fF ++ reverse r, lenF=lenF+lenR, r=[] , lenR=0}

snoc :: a -> Queue a -> Queue a
snoc x (Queue {..}) = queue $ Queue {fF, lenF, r=x:r, lenR=lenR+1}

head :: Queue a -> a
head (Queue {fF=x:xs}) = x
head _ = error "empty"

tail :: Queue a -> Queue a
tail (Queue {fF=x:xs, ..}) = queue $ Queue {fF=xs, lenF=lenF-1, ..}
tail _ = error "empty"

insert :: Int -> Queue String
insert n = foldl' (\q e -> snoc (show e) q) empty [1..n]

insert_and_drain :: Int -> Queue String
insert_and_drain n = let q = foldl' (\q e -> snoc (show e) q) empty [1..n]
                     in foldl' (\q e -> PurelyFunctQueue.tail q) q [1..n-1]

profile :: IO ()
profile = defaultMain [
       bgroup "insert" [
                      bench "insert 1*10**4" $ whnf insert 10000
                    , bench "insert 2*10**4" $ whnf insert 20000
                    , bench "insert 5*10**4" $ whnf insert 50000
                    , bench "insert 1*10**5" $ whnf insert 100000
                    , bench "insert 2*10**5" $ whnf insert 200000
                    , bench "insert 5*10**5" $ whnf insert 500000
                    , bench "insert 1*10**6" $ whnf insert 1000000
                    , bench "insert 2*10**6" $ whnf insert 2000000
                    , bench "insert 5*10**6" $ whnf insert 5000000
                    ],
       bgroup "insert_and_drain" [
                      bench "insert_and_drain 1*10**4" $ whnf insert_and_drain 10000
                    , bench "insert_and_drain 2*10**4" $ whnf insert_and_drain 20000
                    , bench "insert_and_drain 5*10**4" $ whnf insert_and_drain 50000
                    , bench "insert_and_drain 1*10**5" $ whnf insert_and_drain 100000
                    , bench "insert_and_drain 2*10**5" $ whnf insert_and_drain 200000
                    , bench "insert_and_drain 5*10**5" $ whnf insert_and_drain 500000
                    , bench "insert_and_drain 1*10**6" $ whnf insert_and_drain 1000000
                    , bench "insert_and_drain 2*10**6" $ whnf insert_and_drain 2000000
                    , bench "insert_and_drain 5*10**6" $ whnf insert_and_drain 5000000
                    ]
      ]