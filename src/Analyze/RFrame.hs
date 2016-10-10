{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Analyze.RFrame where

import           Analyze.Common      (Data, checkSubset, lookupOrThrow)
import           Analyze.Decoding    (Decoder (..), decoderKeys, runDecoder)
import qualified Control.Foldl       as F
import           Control.Monad.Catch (MonadThrow (..))
import qualified Data.Aeson          as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.HashSet        (HashSet)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- In-memory row-oriented frame
data RFrame k v = RFrame
  { rframeKeys :: !(Vector k)
  , rframeData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance A.ToJSON v => A.ToJSON (RFrame Text v) where
  toJSON frame = A.Array (A.toJSON . HM.fromList . V.toList <$> rframeIter frame)

rframeCols :: RFrame k v -> Int
rframeCols (RFrame ks _) = V.length ks

rframeRows :: RFrame k v -> Int
rframeRows (RFrame _ vs) = V.length vs

rframeIter :: Data k => RFrame k v -> Vector (Vector (k, v))
rframeIter (RFrame ks vs) = V.zip ks <$> vs

rframeFold :: (Data k, Monad m) => F.FoldM m (Vector v) a -> RFrame k v -> m a
rframeFold ff (RFrame kv vs) = F.foldM ff vs

rframeFoldKV :: Monad m => F.FoldM m (Vector (k, v)) a -> RFrame k v -> m a
rframeFoldKV = undefined

rframeDecode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
rframeDecode decoder rframe@(RFrame ks _) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    makeLookup = flip lookupOrThrow . HM.fromList . V.toList
    decoded = runDecoder decoder . makeLookup <$> rframeIter rframe

rframeFilter :: (Int -> Vector (k, v) -> Bool) -> RFrame k v -> RFrame k v
rframeFilter = undefined

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
rframeAppend :: MonadThrow m => RFrame k v -> RFrame k v -> m (RFrame k v)
rframeAppend = undefined
