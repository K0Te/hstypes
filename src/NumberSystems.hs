{-# LANGUAGE InstanceSigs #-}

-- Some number representation examples
module NumberSystems where

class Number a where
  dec :: a -> a
  inc :: a -> a
  add :: a -> a -> a
  sub :: a -> a -> a
  toInt :: a -> Int

-- 0 = zero
-- 1 = succ zero
-- 2 = succ succ zero
type RealChurch a = a -> (a -> a) -> a
zero :: RealChurch a
zero zero succ = zero
one :: RealChurch a
one zero succ = succ zero
two :: RealChurch a
two zero succ = succ $ succ zero

data ChurchNumber = Zero | Succ ChurchNumber deriving Show

instance Number ChurchNumber where
  dec :: ChurchNumber -> ChurchNumber
  dec Zero = error "No negative Church numbers, sorry"
  dec (Succ x) = x

  inc :: ChurchNumber -> ChurchNumber
  inc x = Succ x

  add :: ChurchNumber -> ChurchNumber -> ChurchNumber
  add Zero x = x
  add (Succ x) y = Succ (x `add` y)

  sub :: ChurchNumber -> ChurchNumber -> ChurchNumber
  sub x Zero = x
  sub Zero _ = error "No negative Church numbers, sorry"
  sub (Succ x) (Succ y) = x `sub` y

  toInt :: ChurchNumber -> Int
  toInt Zero = 0
  toInt (Succ x) = 1 + toInt x

-- Alpahbeth : {0, 1}
-- Least significant first, each next position indicates *2
newtype DenseBinary = DenseBinary [Bool]
unpack :: DenseBinary -> [Bool]
unpack (DenseBinary x) = x
recursiveInc []           = [True]
recursiveInc (True  : xs) = False : (recursiveInc xs)
recursiveInc (False : xs) = True : xs
recursiveDec []           = error "No negative binaries in this representation"
recursiveDec (True  : xs) = False : xs
recursiveDec (False : xs) = True : (recursiveDec xs)

instance Number DenseBinary where
  dec :: DenseBinary -> DenseBinary
  dec = DenseBinary . recursiveDec . unpack

  inc :: DenseBinary -> DenseBinary
  inc = DenseBinary . recursiveInc . unpack

  add :: DenseBinary -> DenseBinary -> DenseBinary
  add x y = DenseBinary $ recursiveAdd (unpack x) (unpack y)
    where
      recursiveAdd :: [Bool] -> [Bool] -> [Bool]
      recursiveAdd [] y = y
      recursiveAdd y [] = y
      recursiveAdd (False:xs) (y:ys) = y:(recursiveAdd xs ys)
      recursiveAdd (x:xs) (False:ys) = x:(recursiveAdd xs ys)
      -- carry one using inc !
      recursiveAdd (True:xs) (True:ys) = False:recursiveInc (recursiveAdd xs ys)

  sub :: DenseBinary -> DenseBinary -> DenseBinary
  sub x y = DenseBinary $ recursiveSub (unpack x) (unpack y)
    where
      recursiveSub :: [Bool] -> [Bool] -> [Bool]
      recursiveSub y [] = y
      recursiveSub [] y = error "No negative binaries in this representation"
      recursiveSub (x:xs) (False:ys) = x:(recursiveSub xs ys)
      recursiveSub (True:xs) (True:ys) = False:(recursiveSub xs ys)
      -- borrow one using dec !
      recursiveSub (False:xs) (True:ys) = True:recursiveDec (recursiveSub xs ys)

  toInt :: DenseBinary -> Int
  toInt = recToInt . unpack
    where
      -- Nice alternitive definition
      -- recToInt xs = (sum $ zipWith (\x weight -> if x then weight else 0) xs [2^pwr | pwr <-[0..]])
      recToInt [] = 0
      recToInt (x:xs) = (if x then 1 else 0) + 2 * recToInt xs

-- powers of 2, starting from least significant non-zero value
newtype SparseBinary = SparseBinary [Int] deriving Show
unpackSparse (SparseBinary x) = x

recInc :: Int -> [Int] -> [Int]
recInc x [] = [x]
recInc x (y : ys) | x < y  = x : y : ys
                  | x == y = recInc (x + 1) ys
                  | x > y  = y : recInc x ys
recDec :: Int -> [Int] -> [Int]
recDec x [] = error "No negative numbers here"
recDec x (y : ys) |
  -- n - 2**x = n + 2**x - 2**(x+1)
                    x < y  = x : recDec (x + 1) (y : ys)
                  | x == y = ys
                  | x > y  = y : recDec x ys


-- Now we store only powers of two, where we have 1 bits
instance Number SparseBinary where
  inc :: SparseBinary -> SparseBinary
  inc = SparseBinary . recInc 0 . unpackSparse

  dec :: SparseBinary -> SparseBinary
  dec = SparseBinary . recDec 0 . unpackSparse

  add :: SparseBinary -> SparseBinary -> SparseBinary
  add x y = SparseBinary $ recursiveAdd (unpackSparse x) (unpackSparse y)
    where
      recursiveAdd :: [Int] -> [Int] -> [Int]
      recursiveAdd [] y = y
      recursiveAdd (x:xs) ys = recursiveAdd xs (recInc x ys)

  sub :: SparseBinary -> SparseBinary -> SparseBinary
  sub x y = SparseBinary $ recursiveSub (unpackSparse x) (unpackSparse y)
    where
      recursiveSub :: [Int] -> [Int] -> [Int]
      recursiveSub x [] = x
      recursiveSub [] y = error "No negative numbers here"
      recursiveSub xs (y:ys) = recursiveSub (recDec y xs) ys

  toInt :: SparseBinary -> Int
  toInt = recToInt . unpackSparse
    where
      recToInt xs = sum [2^x | x<-xs]

-- values of 2**k-1, sparse representation, O(1) inc/dec !
newtype SkewBinary = SkewBinary [Int] deriving Show
unpackSkew (SparseBinary x) = x

instance Number SkewBinary where
  inc :: SkewBinary -> SkewBinary
  inc (SkewBinary (x1:x2:xs)) = SkewBinary $ if x1 == x2 then  (x1+x2+1):xs else 1:x1:x2:xs
  inc (SkewBinary []) = SkewBinary [1]

  dec :: SkewBinary -> SkewBinary
  dec (SkewBinary []) = error "No negative numbers here"
  dec (SkewBinary (1:xs)) = SkewBinary xs
  dec (SkewBinary (x:xs)) = let half = x `div` 2 in SkewBinary (half:half:xs)

  add :: SkewBinary -> SkewBinary -> SkewBinary
  add (SkewBinary []) x = x
  add x (SkewBinary []) = x
  add (SkewBinary (x:xs)) (SkewBinary (y:ys)) = if x < y
