{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze.Decoding
  ( Arg(..)
  , Decoder(..)
  , decoderKeys
  , decodeRow
  , decodeCol
  , fromArg
  ) where

import           Analyze.Common           (Data)
import           Control.Applicative.Free (Ap (..), liftAp)
import qualified Control.Foldl            as F
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe)
import           Data.Profunctor          (Profunctor (..))
import           Data.Vector              (Vector)
import qualified Data.Vector              as V

data Arg m k v a = Arg k (F.FoldM m v a) deriving (Functor)

instance Monad m => Profunctor (Arg m k) where
  dimap l r (Arg k f) = Arg k (dimap l r f)

newtype Decoder m k v a = Decoder (Ap (Arg m k v) a) deriving (Functor, Applicative)

fromArg :: Arg m k v a -> Decoder m k v a
fromArg = Decoder . liftAp

decoderKeys :: Decoder m k v a -> [k]
decoderKeys (Decoder x) = go x
  where
    go :: Ap (Arg m k v) a -> [k]
    go (Pure _)            = []
    go (Ap (Arg k _) rest) = k : (go rest)

-- This is pretty sensitive to 'lets'
apRow :: (Data k, Monad m) => Ap (Arg m k v) a -> HashMap k v -> m a
apRow (Pure a) _ = pure a
apRow (Ap (Arg k f) rest) row = do
  let mv = HM.lookup k row
  z <- F.foldM f mv
  fz <- apRow rest row
  return (fz z)

decodeRow :: (Data k, Monad m) => Decoder m k v a -> HashMap k v -> m a
decodeRow (Decoder x) = apRow x

apCol :: (Data k, Monad m) => Ap (Arg m k v) a -> HashMap k (Vector v) -> m a
apCol (Pure a) _ = pure a
apCol (Ap (Arg k f) rest) dat = do
  let vs = fromMaybe (V.empty) (HM.lookup k dat)
  z <- F.foldM f vs
  fz <- apCol rest dat
  return (fz z)

decodeCol :: (Data k, Monad m) => Decoder m k v a -> HashMap k (Vector v) -> m a
decodeCol (Decoder x) = apCol x
