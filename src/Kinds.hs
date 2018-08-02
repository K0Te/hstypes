{-# Language GADTs, TypeFamilies, DataKinds #-}

module Kinds where

data Nat = Zero | Succ Nat deriving Show

type family Add a b where
  Add 'Zero b = b
  Add ('Succ a) b = 'Succ (Add a b)
-- *Kinds> :k Add
-- Add :: Nat -> Nat -> Nat

-- *Kinds> :kind! Add (Succ Zero) (Succ Zero)
-- Add (Succ Zero) (Succ Zero) :: Nat
-- = 'Succ ('Succ 'Zero)

type family IsZero (n::Nat) :: Bool where
  IsZero Zero = 'True
  IsZero _ = 'False

data Vect (n::Nat) a where
  Nil :: Vect 'Zero a
  Cons :: a -> Vect n a -> Vect (Succ n) a

lengthV :: Vect n a -> Nat
lengthV x = Succ Zero -- Bug, no relation between n <--> Nat

data SNat (n::Nat) where
  SZero :: SNat 'Zero
  Ssucc :: SNat n -> SNat (Succ n)
  -- ???

lengthV2 :: Vect n a -> SNat n
lengthV2 Nil = SZero
lengthV2 (Cons x xs) = Ssucc (lengthV2 xs)
-- seems like only one which will type-check ?
-- check len on list of list (1)
len = lengthV2 (Cons (Cons 'a' Nil) Nil) :: SNat ('Succ 'Zero)

-- type family Add2 (a::Int) (b::Int) where
  -- Add2 a b = a + b
