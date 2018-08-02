{-# Language TemplateHaskell, DefaultSignatures, EmptyCase, ExistentialQuantification #-}
{-# Language FlexibleContexts, FlexibleInstances, GADTs, InstanceSigs, DataKinds #-}
{-# Language KindSignatures, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
{-# Language TypeInType, TypeOperators, UndecidableInstances #-}

module SingletonLib where

import Data.Singletons.TH

$(singletons [d|
  data Nat = Zero | Succ Nat deriving Show
  add :: Nat -> Nat -> Nat
  add Zero x = x
  add (Succ x) y = Succ (add x y)
  |])

-- *SingletonLib> :kind!  Add (Succ Zero) (Succ Zero)
-- Add (Succ Zero) (Succ Zero) :: Nat
-- = 'Succ ('Succ 'Zero)

data Vect (n::Nat) a where
  Nil :: Vect 'Zero a
  Cons :: a -> Vect n a -> Vect ('Succ n) a

oldLength :: Vect n a -> Sing n
oldLength Nil = SZero
oldLength (Cons x xs) = SSucc (oldLength xs)

newLength :: forall a n . SingI n => Vect n a -> Sing n
newLength _ = (sing :: SNat n) -- works even w/o this signature

-- *SingletonLib> :t newLength (Cons 'a' (Cons 'b' Nil))
-- newLength (Cons 'a' (Cons 'b' Nil)) :: Sing ('Succ ('Succ 'Zero))

newLength2 :: forall a n . SingI n => Vect n a -> Nat
newLength2 _ = fromSing (sing :: SNat n)