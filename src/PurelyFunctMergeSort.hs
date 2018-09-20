{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module PurelyFunctMergeSort where

import           Criterion.Main
import           Data.List                      ( foldl' )

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xso@(x : xs) yso@(y : ys) =
  if x <= y then x : merge xs yso else y : merge xso ys

data MergeSortContainer a where
 MergeSortContainer :: Ord a => {len :: Int, segments :: [[a]]} -> MergeSortContainer a

deriving instance (Show a) => Show (MergeSortContainer a)

add :: a -> MergeSortContainer a -> MergeSortContainer a
add a MergeSortContainer {..} = MergeSortContainer
  (len + 1)
  (addSegment [a] segments len)
 where
  addSegment :: Ord a => [a] -> [[a]] -> Int -> [[a]]
  addSegment el lst size
    | (size `mod` 2) == 0 = el : lst
    | otherwise = addSegment (merge el (head lst)) (tail lst) (size `div` 2)

sort :: MergeSortContainer a -> [a]
sort MergeSortContainer {..} = foldl' merge [] segments

insert_and_sort :: Int -> [Int]
insert_and_sort n = sort $ foldl' (\c e -> add (e+1) c) (MergeSortContainer 0 []) (reverse [1..n])


profile :: IO ()
profile = defaultMain
  [ bgroup
    "insert_and_sort"
    [ bench "insert_and_sort 1*10**4" $ whnf insert_and_sort 10000
    , bench "insert_and_sort 2*10**4" $ whnf insert_and_sort 20000
    , bench "insert_and_sort 5*10**4" $ whnf insert_and_sort 50000
    , bench "insert_and_sort 1*10**5" $ whnf insert_and_sort 100000
    , bench "insert_and_sort 2*10**5" $ whnf insert_and_sort 200000
    , bench "insert_and_sort 5*10**5" $ whnf insert_and_sort 500000
    , bench "insert_and_sort 1*10**6" $ whnf insert_and_sort 1000000
    , bench "insert_and_sort 2*10**6" $ whnf insert_and_sort 2000000
    , bench "insert_and_sort 5*10**6" $ whnf insert_and_sort 5000000
    ]
  ]
