{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module PurelyFuncRTDeque where

import           Criterion.Main
import           Data.List                      ( foldl' )

data Deque a = Deque
  { forw :: [a]
  , lenF :: Int
  , suspF :: ![a]
  , rev  :: [a]
  , lenR :: Int
  , suspR :: ![a]} deriving Show

empty :: Deque a
empty = Deque {forw = [], lenF = 0, suspF = [], rev = [], lenR = 0, suspR = []}

isEmpty :: Deque a -> Bool
isEmpty Deque {..} = lenF + lenR == 0

rotateRev :: [a] -> [a] -> [a] -> [a]
rotateRev [] f acc = reverse f ++ acc
rotateRev (r : rs) f a =
  r : rotateRev rs (drop c f) ((reverse $ take c f) ++ a)

rotateDrop :: [a] -> Int -> [a] -> [a]
rotateDrop r i f
  | i < c     = rotateRev r (drop i f) []
  | otherwise = let s : rs = r in s : rotateDrop rs (i - c) (drop c f)

exec1 :: [a] -> [a]
exec1 (x : xs) = x `seq` xs
exec1 x        = x

exec2 :: [a] -> [a]
exec2 = exec1 . exec1

c :: Int
c = 2 -- or 3, but not more or less
-- Invarians:
-- lenF == |forw|
-- lenR == |rev|
-- |forw| <= c*|rev| + 1
-- |rev| <= c*|forw| + 1
queue :: Deque a -> Deque a
queue Deque {..}
  | lenF > c * lenR + 1
  = let mid  = (lenF + lenR) `div` 2
        remR = (lenF + lenR) - mid
        r'   = rotateDrop rev mid forw
        f'   = take mid forw
    in  Deque
          { forw  = f'
          , suspF = f'
          , lenF  = mid
          , rev   = rev ++ reverse (drop mid forw)
          , lenR  = remR
          , suspR = r'
          }
  | lenR > c * lenF + 1
  = let mid  = (lenF + lenR) `div` 2
        remF = (lenF + lenR) - mid
        r'   = take mid rev
        f'   = rotateDrop forw mid rev
    in  Deque
          { forw  = f'
          , suspF = f'
          , lenF  = remF
          , rev   = rev ++ reverse (drop mid forw)
          , lenR  = mid
          , suspR = r'
          }
  | otherwise
  = Deque {..}

snoc :: Deque a -> a -> Deque a
snoc Deque {..} x = queue Deque
  { rev   = x : rev
  , lenR  = lenR + 1
  , suspF = exec1 suspF
  , suspR = exec1 suspR
  , ..
  }

head :: Deque a -> a
head (isEmpty -> True)         = error "empty"
head Deque { forw = x : _, ..} = x
head Deque { rev = x : _, ..}  = x

tail :: Deque a -> Deque a
tail (isEmpty -> True)          = error "empty"
tail Deque { forw = x : xs, ..} = queue Deque
  { forw  = xs
  , lenF  = lenF - 1
  , suspF = exec2 suspF
  , suspR = exec2 suspR
  , ..
  }
tail Deque { forw = [], rev = _, ..} = empty

insert :: Int -> Deque String
insert n = foldl' (\q e -> snoc q (show e)) empty [1 .. n]

insert_and_drain :: Int -> Deque String
insert_and_drain n =
  let q = foldl' (\q e -> (flip snoc) (show e) q) empty [1 .. n]
  in  foldl' (\q e -> PurelyFuncRTDeque.tail q) q [1 .. n - 1]

-- Twice slow compared to PurelyFunctQueue :(
-- However, still O(n) !
profile :: IO ()
profile = defaultMain
  [ bgroup
    "insert"
    [ bench "insert 1*10**4" $ whnf insert 10000
    , bench "insert 2*10**4" $ whnf insert 20000
    , bench "insert 5*10**4" $ whnf insert 50000
    , bench "insert 1*10**5" $ whnf insert 100000
    , bench "insert 2*10**5" $ whnf insert 200000
    , bench "insert 5*10**5" $ whnf insert 500000
    , bench "insert 1*10**6" $ whnf insert 1000000
    , bench "insert 2*10**6" $ whnf insert 2000000
    , bench "insert 5*10**6" $ whnf insert 5000000
    ]
  , bgroup
    "insert_and_drain"
    [ bench "insert_and_drain 1*10**4" $ whnf insert_and_drain 10000
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
