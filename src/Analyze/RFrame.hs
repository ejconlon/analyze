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
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- In-memory row-oriented frame
data RFrame k v = RFrame
  { _rframeKeys   :: !(Vector k)
  , _rframeLookup :: !(HashMap k Int)
  , _rframeData   :: !(Vector (Vector v))
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

numCols :: RFrame k v -> Int
numCols (RFrame ks _ _) = V.length ks

numRows :: RFrame k v -> Int
numRows (RFrame _ _ vs) = V.length vs

iter :: Eq k => RFrame k v -> Vector (Vector (k, v))
iter (RFrame ks _ vs) = V.zip ks <$> vs

decode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
decode decoder rframe@(RFrame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

filter :: Data k => RFrameFilter k v -> RFrame k v -> RFrame k v
filter p (RFrame ks look vs) = RFrame ks look vs'
  where
    vs' = V.ifilter (p look) vs

update :: (Data k, MonadThrow m) => RFrameUpdate k v -> RFrame k v -> m (RFrame k v)
update (RFrameUpdate uks uvs) (RFrame fks look fvs) = do
  let fSize = V.length fvs
      uSize = V.length uvs
  if fSize /= uSize
    then throwM (RowSizeMismatch fSize uSize)
    else do
      checkForDupes uks
      let kis = mergeKeys fks uks
          ks' = (\(k, _, _) -> k) <$> kis
          look' = makeLookup ks'
          vs' = V.zipWith (runIndexedLookup kis) fvs uvs
      return (RFrame ks' look' vs')

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
appendRows :: (Data k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
appendRows (RFrame ks0 look0 vs0) (RFrame ks1 look1 vs1) = do
  checkReorder ks0 ks1
  let vs1' = reorder ks0 look1 vs1
  return (RFrame ks0 look0 (vs0 V.++ vs1'))

extendCols :: (Data k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
extendCols f g = update (toUpdate g) f
