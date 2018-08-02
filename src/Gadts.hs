{-# Language GADTs, TypeFamilies #-}
module Gadts where

-- Based on youtube "Making sense of the Haskell type system by Ryan Lemmer at FnConf17"

data Maybe' a = Nothing' | Just' a

data List a = NilL | ConsL (List a)

data Vect1 n a = Nil1 | Cons1 a (Vect1 n a)

data Zero = Zero_ deriving Show

data Succ n = Succ_ n deriving Show

zero = Zero_
one = Succ_ Zero_
two = Succ_ (Succ_ Zero_)

lol = Succ_ False
lol2 = Succ_ 123

w1 = Nil1 :: Vect1 Zero Int -- Nice ?
w2 = Cons1 'a' Nil1 :: Vect1 (Succ Zero) Char -- ok

lol_2 = Nil1 :: Vect1 (Succ Bool) Char -- Wrong size :(

-- GADTs
data Vect n a where
  Nil :: Vect Zero a
  Cons :: a -> Vect n a -> Vect (Succ n) a

instance Show a => Show (Vect n a) where
  show Nil = "Nil"
  show (Cons x xs) = (show x) ++ ":" ++ show xs

x = Cons 'a' Nil :: Vect (Succ Zero) Char
y = Cons 'a' (Cons 'b' Nil) :: Vect (Succ (Succ Zero)) Char

-- Wow !
zipV :: Vect n a -> Vect n b -> Vect n (a, b)
zipV Nil Nil = Nil
zipV (Cons x xs) (Cons y ys) = Cons (x, y) (zipV xs ys)

-- zipV Nil (Cons 'a' Nil) -> is rejected at compile time !

stillBad = Succ_ False :: Succ Bool

-- open Type Family

type family Add1 a b
type instance Add1 Zero b = b
type instance Add1 (Succ a) b = Succ (Add1 a b)

-- Wow !
-- *Gadts> :kind! Add1 (Succ Zero) (Succ Zero)
-- Add1 (Succ Zero) (Succ Zero) :: *
-- = Succ (Succ Zero)

-- closed type families

type family Add a b where
  Add Zero b = b
  Add (Succ a) b = Succ (Add a b)

append :: Vect n a -> Vect m a -> Vect (Add n m) a
append Nil x = x
append (Cons x xs) ys = Cons x (append xs ys)

xx = append (Cons 'a' (Cons 'b' Nil)) (Cons 'c' Nil) :: Vect (Succ (Succ (Succ Zero))) Char
