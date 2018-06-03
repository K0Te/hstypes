{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ViewPatterns, RankNTypes, TypeFamilies, ImplicitParams #-}
module Lib
    ( someFunc, SomeC(..), rollDie, Die(..), xt, fib
    ) where

import Control.Exception
import Control.DeepSeq
import GHC.Generics (Generic, Generic1)

import           Data.Aeson
import           Data.Aeson.Types
import Data.Char
import Control.Applicative (liftA3, liftA2)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

someFunc :: IO ()
someFunc = do
  (putStrLn . show) =<< (recEval2 (TmSucc $ TmPred $ TmSucc TmZero))
  (putStrLn . show) =<< recEval2 TmFalse
  (putStrLn . show) =<< (recEval2 TmTrue)
  (putStrLn . show) =<< (recEval2 (TmSucc TmZero))
  (putStrLn . show) =<< (recEval2 (TmSucc $ TmSucc $ TmSucc TmZero))
  (putStrLn . show) =<< (recEval2 (TmPred $ TmSucc $ TmPred $ TmIsZero TmZero))
  (putStrLn . show) =<< (recEval2 (TmSucc $ TmPred $ TmSucc $ TmPred $ TmZero))
  (putStrLn . show) =<< (recEval2 $ TmIf TmFalse (TmSucc TmZero) TmFalse)
  (putStrLn . show) =<< (recEval2 $ TmIf TmTrue (TmSucc TmZero) TmFalse)
  putStrLn $ xxx 0
  putStrLn $ xxx 1
  putStrLn $ show $ irref (Just 123)
  putStrLn $ show $ irref Nothing
  putStrLn $ show $ encoded
  -- putStrLn $ show $ let ?cmp = (<=) in sort [3,1,2::Int]


recEval t = catch (do let t' = evalStep t in t' `deepseq` recEval t') (\(ThisException _) -> return t)
recEval2 t = tryJust (\(ThisException x) -> Just x) (do let t2 = bigEval t in t2 `deepseq` return t2)

data Term = TmTrue | TmFalse | TmIf Term Term Term | TmZero | TmSucc Term | TmPred Term | TmIsZero Term
      deriving (Eq, Show, Generic, NFData)

data MyException = ThisException Term
    deriving (Eq, Show)
instance Exception MyException

is_num TmZero = True
is_num (TmSucc t) = is_num t
is_num _ = False

evalStep :: Term -> Term
evalStep (TmIf TmTrue t1 _) = t1
evalStep (TmIf TmFalse _ t2) = t2
evalStep (TmIf x t1 t2) = TmIf (evalStep x) t1 t2
evalStep (TmSucc t) = TmSucc (evalStep t)
evalStep (TmPred TmZero) = TmZero
evalStep (TmPred (TmSucc t)) = if is_num t then t else TmPred (TmSucc (evalStep t))
evalStep (TmPred t1) = TmPred (evalStep t1)
evalStep (TmIsZero TmZero) = TmTrue
evalStep (TmIsZero (TmSucc t)) = if is_num t then TmFalse else TmIsZero (TmSucc (evalStep t))
evalStep (TmIsZero t) = TmIsZero (evalStep t)
evalStep t = throw $ ThisException t


bigEval :: Term -> Term
bigEval t
  | isTrivial t = t
  | (TmIsZero x) <- t, bigEval x == TmZero = TmTrue
  | (TmIsZero x) <- t, is_num (bigEval x) = TmFalse
  | (TmSucc x) <- t, is_num (bigEval x) = TmSucc (bigEval x)
  | (TmPred x) <- t, bigEval x == TmZero = TmZero
  | (TmPred x) <- t, is_num (bigEval x), (TmSucc xx) <- bigEval x = bigEval xx
  | (TmIf c t1 t2) <- t, TmTrue <- (bigEval c) = bigEval t1
  | (TmIf c t1 t2) <- t, TmFalse <- (bigEval c) = bigEval t2
  | True = throw $ ThisException t
  where
    isTrivial TmFalse = True
    isTrivial TmTrue = True
    isTrivial TmZero = True
    isTrivial _ = False

view :: Int -> String
view 0 = "Zero"
view 1 = "One"
view _ = "More"


xxx :: Int -> String
xxx (view -> "Zero") = "Is a zero"
xxx (view -> _) = "Is not zero"


irref :: Maybe Int -> Int
irref ~(Just x) = 123


type IdFunc = forall a. a -> a
mid :: IdFunc
mid x = x

someInt :: IdFunc -> Integer
someInt idf = idf 101

newtype SomeC a =
    SomeC {
      runSomeC ::
          forall r.
          ((a ~ Int) => Int -> r) ->
          ((a ~ Char) => Char -> r) ->
          (a -> r) ->
          r
    }

-- x = SomeC (\n -> (\c -> (\o -> (\z->z))))



-- sortBy :: (a -> a -> Bool) -> [a] -> [a]
-- sortBy f lst = zip lst (fmap f lst)

-- sort   :: (?cmp :: a -> a -> Bool) => [a] -> [a]
-- sort    = sortBy ?cmp


data Foo = FooBar { bar:: Bar, baz :: Baz} deriving Generic

data Bar = Bar Integer deriving Generic

newtype Baz = Baz Integer deriving Generic

instance ToJSON Foo where
  toJSON = genericToJSON options

instance ToJSON Bar where
  toJSON = genericToJSON option_nested

instance ToJSON Baz where
  toJSON = genericToJSON option_nested

options :: Options
options = defaultOptions { tagSingleConstructors = True,
                           sumEncoding = ObjectWithSingleField,
                           fieldLabelModifier = \s -> (toUpper . head) s : (tail s)  }
option_nested :: Options
option_nested = defaultOptions { sumEncoding = UntaggedValue }

foo :: Foo
foo = FooBar (Bar 4) (Baz 2)

encoded = encode foo

data Die = One | Two deriving (Eq, Show)
intToDie :: Int -> Die
intToDie n
  | n < 3 = One
  | True = Two


rollDie :: State StdGen Die
-- rollDie = state $ do
--   (n, s) <- randomR (1, 6)
--   return (intToDie n, s)
rollDie = fmap intToDie $ state (randomR (1,6))


rollsToGetN :: Int -> State StdGen (Int, Int)
rollsToGetN n = liftA2 (,) (state $ randomR (0, 6)) (state $ randomR (0, 6))

xt :: State Int Int
xt = do
  s <- get
  put (s+2)
  s1 <- get
  return 9

fib :: Int -> State (Int, Int) Int
fib n = do
  (x1, x2) <- get
  put (x2, x2+x1)
  if n == 0 then return x2 else fib (n-1)



