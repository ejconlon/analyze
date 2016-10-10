{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Analyze.Decoding
  ( Decoder(..)
  , decoderKeys
  , require
  , requireWhere
  , runDecoder
  ) where

import           Analyze.Common           (Data)
import           Control.Applicative.Free (Ap (..), liftAp)
import           Data.Maybe               (fromMaybe)
import           Data.Profunctor          (Profunctor (..))

data Arg m k v a = Arg k (v -> m a) deriving (Functor)

instance Functor m => Profunctor (Arg m k) where
  dimap l r (Arg k f) = Arg k (\u -> r <$> f (l u))

newtype Decoder m k v a = Decoder (Ap (Arg m k v) a) deriving (Functor, Applicative)

fromArg :: Arg m k v a -> Decoder m k v a
fromArg = Decoder . liftAp

require :: Applicative m => k -> Decoder m k v v
require k = fromArg (Arg k pure)

requireWhere :: k -> (k -> v -> m a) -> Decoder m k v a
requireWhere k e = fromArg (Arg k (e k))

decoderKeys :: Data k => Decoder m k v a -> [k]
decoderKeys (Decoder x) = go x
  where
    go :: Ap (Arg m k v) a -> [k]
    go (Pure _)            = []
    go (Ap (Arg k _) rest) = k : go rest

-- This is pretty sensitive to 'lets'
apRow :: (Data k, Monad m) => Ap (Arg m k v) a -> (k -> m v) -> m a
apRow (Pure a) _ = pure a
apRow (Ap (Arg k f) rest) row = do
  v <- row k
  z <- f v
  fz <- apRow rest row
  return (fz z)

runDecoder :: (Data k, Monad m) => Decoder m k v a -> (k -> m v) -> m a
runDecoder (Decoder x) = apRow x
