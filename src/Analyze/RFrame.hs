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

instance A.ToJSON v => A.ToJSON (RFrame Text v) where
  toJSON frame = A.Array (A.toJSON . HM.fromList . V.toList <$> iter frame)

data RFrameUpdate k v = RFrameUpdate
  { _rframeUpdateKeys :: !(Vector k)
  , _rframeUpdateData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor)

-- TODO ToJSON and FromJSON for RFrameUpdate

type RFrameFilter k v = HashMap k Int -> Int -> Vector v -> Bool

empty :: RFrame k v
empty = RFrame V.empty HM.empty V.empty

fromUpdate :: (Data k, MonadThrow m) => RFrameUpdate k v -> m (RFrame k v)
fromUpdate (RFrameUpdate ks vs) = checkForDupes ks >> pure (RFrame ks (makeLookup ks) vs)

toUpdate :: Data k => RFrame k v -> RFrameUpdate k v
toUpdate (RFrame ks _ vs) = RFrameUpdate ks vs

cols :: RFrame k v -> Int
cols (RFrame ks _ _) = V.length ks

rows :: RFrame k v -> Int
rows (RFrame _ _ vs) = V.length vs

iter :: Eq k => RFrame k v -> Vector (Vector (k, v))
iter (RFrame ks _ vs) = V.zip ks <$> vs

decode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
decode decoder rframe@(RFrame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

-- bind :: (Data k, MonadThrow m) => Decoder m k v (RFrameUpdate k v) -> RFrame k v -> m (RFrame k v)
-- bind decoder rframe@(RFrame ks look vs) = undefined

filter :: Data k => RFrameFilter k v -> RFrame k v -> RFrame k v
filter = undefined

update :: (Data k, MonadThrow m) => RFrameUpdate k v -> RFrame k v -> m (RFrame k v)
update = undefined

dropCols :: Data k => HashSet k -> RFrame k v -> RFrame k v
dropCols names (RFrame ks look vs) = RFrame ks' look' vs'
  where
    (_, ks') = V.partition (flip HS.member names) ks
    look' = makeLookup ks'
    vs' = assemble ks' look <$> vs

keepCols :: Data k => HashSet k -> RFrame k v -> RFrame k v
keepCols names (RFrame ks look vs) = RFrame ks' look' vs'
  where
    (ks', _) = V.partition (flip HS.member names) ks
    look' = makeLookup ks'
    vs' = assemble ks' look <$> vs

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
append :: MonadThrow m => RFrame k v -> RFrame k v -> m (RFrame k v)
append = undefined
