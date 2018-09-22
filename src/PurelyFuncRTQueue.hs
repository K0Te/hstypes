{-# LANGUAGE RecordWildCards #-}

module PurelyFuncRTQueue where

import Criterion.Main
import Data.List (foldl')

data Queue a = Queue {
    forw :: [a]
  , rev :: ![a]
  , schedule :: [a]
} deriving Show

empty :: Queue a
empty = Queue {forw=[], rev=[], schedule=[]}

isEmpty :: Queue a -> Bool
isEmpty Queue{..} = null forw

-- non-exaustive pattern, but it's fine as internal part of module
rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y:ys) a = y:a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)

queue :: Queue a -> Queue a
queue Queue{..} = case schedule of
  (s:ss) -> Queue{schedule=ss, ..}
  _      -> let !f' = rotate forw rev [] in Queue{forw=f', rev=[], schedule=f'}

snoc :: Queue a -> a -> Queue a
snoc Queue{..} el = queue Queue{rev=el:rev, ..}

head :: Queue a -> a
head Queue{..} = case forw of
  x:_ -> x
  _ -> error "Empty"

tail :: Queue a -> Queue a
tail Queue{..} = case forw of
  _:xs -> queue Queue{forw=xs, ..}
  _ -> error "Empty"

insert :: Int -> Queue String
insert n = foldl' (\q e -> snoc q (show e)) empty [1..n]

insert_and_drain :: Int -> Queue String
insert_and_drain n = let q = foldl' (\q e -> (flip snoc) (show e) q) empty [1..n]
                     in foldl' (\q e -> PurelyFuncRTQueue.tail q) q [1..n-1]

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