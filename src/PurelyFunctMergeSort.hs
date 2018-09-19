{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module PurelyFunctMergeSort where

import Data.List (foldl')

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xso@(x:xs) yso@(y:ys) = if x <= y then x : merge xs yso else y : merge xso ys

data MergeSortContainer a where
 MergeSortContainer :: Ord a => {len :: Int, segments :: [[a]]} -> MergeSortContainer a

deriving instance (Show a) => Show (MergeSortContainer a)

add :: a -> MergeSortContainer a -> MergeSortContainer a
add a MergeSortContainer{..} = MergeSortContainer (len+1) (addSegment [a] segments len)
  where addSegment :: Ord a => [a] -> [[a]] -> Int -> [[a]]
        addSegment el lst size | (size `mod` 2) == 0 = el : lst
                               | otherwise = addSegment (merge el (head lst)) (tail lst) (size `div` 2)

sort :: MergeSortContainer a -> [a]
sort MergeSortContainer{..} = foldl' merge [] segments