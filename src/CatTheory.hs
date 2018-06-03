module CatTheory where

import Data.Bifunctor

data PreList a b = Nil | Cons a b deriving Show

data Fix f = Fix (f (Fix f))
data LL a = LL (PreList (LL a) a)

instance Bifunctor PreList where
  bimap f g Nil = Nil
  bimap f g (Cons a b) = Cons (f a) (g b)

x :: LL Int
x = LL (Cons (LL $ Cons (LL $ Cons (LL Nil) 3) 2) 2)