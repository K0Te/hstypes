{-# LANGUAGE RankNTypes #-}
module Main where

myFree = Free [Pure Read]
main = run myFree >> return ()

run :: (Free [] Op) -> IO ()
run = foldFree morph >> return ()
	where
		morph :: (Op) -> IO ()
		morph = undefined

data Free f a = Pure a | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
    fmap f (Pure a) = Pure $ f a
    fmap f (Free xf) = Free ((fmap. fmap) f xf)

instance (Functor f) => Applicative (Free f) where
    pure = Pure
    (Pure f) <*> fx = f <$> fx
    (Free ffx) <*> fx = Free $ fmap (<*> fx) ffx

instance (Functor f) => Monad (Free f) where
    (Pure a) >>= fa_mfb = fa_mfb a
    (Free freer) >>= k = Free $ fmap (>>= k) freer

liftF :: Functor f => f a -> Free f a
liftF f = Free $ fmap Pure f

foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree fama (Pure a) = return a
foldFree k (Free freer) = k freer >>= foldFree k

data Op = Take String | Read | Return deriving Show

