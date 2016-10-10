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
  } deriving (Eq, Show, Functor)

data RFrameUpdate k v = RFrameUpdate
  { _rframeUpdateKeys :: !(Vector k)
  , _rframeUpdateData :: !(Vector (Vector v))
  } deriving (Eq, Show)

data RFrameChange m k v = RFrameChange
  { _rframeChangeKeys :: !(Vector k)
  , _rframeChangeDecoder :: !(Decoder m k v (Vector v))
  }

data RFrameDrop k = RFrameDrop
  { _rframeDropKeys :: !(HashSet k)
  } deriving (Eq, Show)

data RFrameKeep k = RFrameKeep
  { _rframeKeepKeys :: !(Vector k)
  } deriving (Eq, Show)

data RFrameFilter m k v = RFrameFilter
  { _rframeFilterFun :: !(Int -> HashMap k Int -> Vector v -> m Bool)
  }

data RFrameMod m k v =
    RFrameModChange !(RFrameChange m k v)
  | RFrameModFilter !(RFrameFilter m k v)
  | RFrameModDrop !(RFrameDrop k)
  | RFrameModKeep !(RFrameKeep k)

data RFrameView k v = RFrameView
  { _rframeViewBase :: !(RFrame k v)
  , _rframeViewUpdates :: !(Vector (RFrameUpdate k v))
  , _rframeViewCols :: !Int
  } deriving (Eq, Show)

makeViewLookup :: (Data k, MonadThrow m) => RFrameView k v -> m (HashMap k Int)
makeViewLookup = undefined

rframeFromView :: (Data k, MonadThrow m) => RFrameView k v -> m (RFrame k v)
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

rframeDecode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
rframeDecode decoder rframe@(RFrame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

-- rframeFilter :: (Int -> Vector (k, v) -> Bool) -> RFrame k v -> RFrame k v
-- rframeFilter = undefined

rframeUpdate :: (Data k, MonadThrow m) => RFrameUpdate k v -> RFrame k v -> m (RFrameView k v)
rframeUpdate = undefined

rframeDrop :: (Data k, MonadThrow m) => RFrameDrop k -> RFrame k v -> m (RFrameView k v)
rframeDrop = undefined

rframeKeep :: (Data k, MonadThrow m) => RFrameDrop k -> RFrame k v -> m (RFrameView k v)
rframeKeep = undefined

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
rframeAppend :: MonadThrow m => RFrame k v -> RFrame k v -> m (RFrame k v)
rframeAppend = undefined
