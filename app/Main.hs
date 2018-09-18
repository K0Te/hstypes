{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Monoid
import Text.Parsec (runParser)
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader  as R
import qualified Control.Monad.Trans.Maybe  as M
import Lib
import HList
import Queue
import Lambda
import PurelyFunctQueue

main :: IO ()
main = profile

-- mainL :: IO ()
-- mainL = do
  -- putStrLn $ showTerm ctx term
  -- res <- reval ctx term
  -- putStrLn $ showTerm ctx res
  -- print $ runParser termParser ["xxx"]  "<xxx>" "\\x.x \\x.x"
  -- where term = App Info (Abs Info "x" $ Var Info 0 2) (Abs Info "z" $ Var Info 1 2)
  --       ctx = [("x", Binding)]

newtype Moi s a = Moi { runMoi :: s -> (a, s) }
instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, s2) = g s in (f a, s2)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> let (a, s1) = g s
                                        (fa, s2) = f s1 in (fa a, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, s1) = f s
                                  (Moi z) = g a in z s1

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: a -> Moi a ()
put a = Moi $ \s -> ((), a)

exec :: Moi s a -> s -> s
exec (Moi sa) s = let (a, s2) = sa s in s2

eval :: Moi s a -> s -> a
eval (Moi sa) s = let (a, s2) = sa s in a

modify :: (s -> s) -> Moi s ()
modify fs = Moi $ \s -> ((), fs s)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap.fmap) f ma

instance Monad m => Applicative (MaybeT m) where
  pure a = MaybeT (pure (pure a))
  (<*>) :: MaybeT m (a->b) -> MaybeT m a -> MaybeT m b
  -- (MaybeT mfa) <*> (MaybeT ma) = MaybeT $ join $ (pure $ \mb_a -> fmap (\mb_f -> mb_f <*> mb_a) mfa) <*> ma
  (MaybeT mfa) <*> (MaybeT ma) = MaybeT $ (fmap (\mb_f -> \mb_a -> mb_f <*> mb_a) mfa) <*> ma


instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= fa_mb = MaybeT $ join $ fmap (convx) $ (fmap.fmap) runMaybeT $ (fmap.fmap) fa_mb ma
    where convx Nothing = pure Nothing
          convx (Just m)  = m

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT r) = EitherT $ (fmap . fmap) f r

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ (pure.pure) a
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT mefa) <*> (EitherT mea) = EitherT $ (fmap (\efa -> \mfa -> efa <*> mfa) mefa) <*> mea

-- Can we use sequenceA here ?)
instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f = EitherT $ join $ fmap (convx) $ (fmap.fmap) runEitherT $ (fmap.fmap) f mea
    where convx (Left x) = pure (Left x)
          convx (Right m)  = m

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea
  where swapEither (Left x) = Right x
        swapEither (Right x) = Left x

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fa fb (EitherT me) = do
  e <- me
  case e of (Left a) -> fa a
            (Right b) -> fb b

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT fr) = ReaderT $ \r -> fmap f (fr r)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \r -> pure a
  (ReaderT ffa) <*> (ReaderT fa) = ReaderT $
    \r -> let mfa = ffa r
              ma = fa r
              in mfa <*> ma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT fma) >>= f = ReaderT $ \r -> do
    reader <- (fmap f (fma r))
    runReaderT reader r

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a->b) -> StateT s m a -> StateT s m b
  fmap f (StateT sm) = StateT $ \s -> let ma = sm s in fmap (\(a, s1) -> (f a, s1)) ma

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a->b) -> StateT s m a -> StateT s m b
  (StateT smf) <*> (StateT sma) = StateT $ \s ->
    let msf = smf s in join $ fmap (\(fa, s1) -> let msa = sma s1 in fmap (\(a, s2) -> (fa a, s2)) msa) msf

instance Monad m => Monad (StateT s m) where
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s) <- sma s
    runStateT (f a) s

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT (const (return $ Right (Just 1)))


-- embedded = undefined
instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const


instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap Right


instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO ac = lift $ liftIO ac

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO ac = MaybeT $ fmap Just $ liftIO ac

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT r m) where
  liftIO :: IO a -> StateT s m a
  liftIO ac =  StateT $ \s -> fmap (\a -> (a, s)) (liftIO ac)

rDec :: Num a => R.Reader a a
rDec = R.ReaderT $ Identity . (1-)

rShow :: Show a => R.ReaderT a Identity String
rShow = R.ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => R.ReaderT a IO a
rPrintAndInc = R.ReaderT $ \r -> do
  putStrLn $ show r
  return (r+1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn $ "Hi: " <> show s
  return (show s, s+1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: M.MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- M.runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)






