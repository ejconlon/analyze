{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Analyze.RFrame where

import           Analyze.Common
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
  { _rframeKeys :: !(Vector k)
  , _rframeLookup :: !(HashMap k Int)
  , _rframeData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data RFrameUpdate k v = RFrameUpdate
  { _rframeUpdateKeys :: !(Vector k)
  , _rframeUpdateData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data RFrameDrop k v = RFrameDrop
  { _rframeDropKeys :: !(HashSet k)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data RFrameMod k v =
    RFrameModUpdate !(RFrameUpdate k v)
  | RFrameModDrop !(RFrameDrop k v)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data RFrameView k v = RFrameView
  { _rframeViewBase :: !(RFrame k v)
  , _rframeViewUpdates :: !(Vector (RFrameUpdate k v))
  , _rframeViewCols :: !Int
  } deriving (Eq, Show, Functor, Foldable, Traversable)

rframeLookup :: (Data k, MonadThrow m) => RFrame k v -> Vector v -> k -> m v
rframeLookup (RFrame _ look _) vs k =
  case HM.lookup k look >>= (vs V.!?) of
    Nothing -> throwM (MissingKeyError k)
    Just v -> pure v

viewLookup :: (Data k, MonadThrow m) => RFrameView k v -> Vector v -> k -> m v
viewLookup = undefined

rframeFromView :: RFrameView k v -> m (RFrame k v)
rframeFromView = undefined

rframeEmpty :: RFrame k v
rframeEmpty = RFrame V.empty HM.empty V.empty

rframeFromUpdate :: (Data k, MonadThrow m) => RFrameUpdate k v -> m (RFrame k v)
rframeFromUpdate (RFrameUpdate ks vs) = checkForDupes ks >> pure (RFrame ks (makeLookup ks) vs)

instance A.ToJSON v => A.ToJSON (RFrame Text v) where
  toJSON frame = A.Array (A.toJSON . HM.fromList . V.toList <$> rframeIter frame)

rframeCols :: RFrame k v -> Int
rframeCols (RFrame ks _ _) = V.length ks

rframeRows :: RFrame k v -> Int
rframeRows (RFrame _ _ vs) = V.length vs

rframeIter :: Eq k => RFrame k v -> Vector (Vector (k, v))
rframeIter (RFrame ks _ vs) = V.zip ks <$> vs

rframeFold :: (Eq k, Monad m) => F.FoldM m (Vector (k, v)) a -> RFrame k v -> m a
rframeFold ff = F.foldM ff . rframeIter

rframeDecode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
rframeDecode decoder rframe@(RFrame ks _ vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . rframeLookup rframe <$> vs

rframeFilter :: (Int -> Vector (k, v) -> Bool) -> RFrame k v -> RFrame k v
rframeFilter = undefined

-- Apply an update
rframeUpdate :: RFrameUpdate k v -> RFrame k v -> m (RFrame k v)
rframeUpdate = undefined

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
rframeAppend :: MonadThrow m => RFrame k v -> RFrame k v -> m (RFrame k v)
rframeAppend = undefined
