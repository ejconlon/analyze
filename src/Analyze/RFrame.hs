{-# LANGUAGE DeriveFunctor     #-}
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

data RFrameUpdate k v = RFrameUpdate
  { _rframeUpdateKeys :: !(Vector k)
  , _rframeUpdateData :: !(Vector (Vector v))
  } deriving (Eq, Show, Functor)

type RFrameMap k v a = Vector k -> HashMap k Int -> Int -> Vector v -> a

type RFrameFilter k v = RFrameMap k v Bool

type RFrameBind k v w = RFrameMap k v (Vector (Vector w))

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

col :: (Data k, MonadThrow m) => k -> RFrame k v -> m (Vector v)
col k (RFrame _ look vs) = V.mapM (\v -> runLookup look v k) vs

decode :: (Data k, MonadThrow m) => Decoder m k v a -> RFrame k v -> m (Vector (m a))
decode decoder rframe@(RFrame ks look vs) = checkSubset required keySet >> pure decoded
  where
    keySet = HS.fromList (V.toList ks)
    required = decoderKeys decoder
    decoded = runDecoder decoder . runLookup look <$> vs

filter :: Data k => RFrameFilter k v -> RFrame k v -> RFrame k v
filter p (RFrame ks look vs) = RFrame ks look vs'
  where
    vs' = V.ifilter (p ks look) vs

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

splitCols :: Data k => (k -> Bool) -> RFrame k v -> (RFrame k v, RFrame k v)
splitCols p (RFrame ks look vs) = (RFrame keepKs keepLook keepVs, RFrame dropKs dropLook dropVs)
  where
    (keepKs, dropKs) = V.partition p ks
    keepLook = makeLookup keepKs
    keepVs = assemble keepKs look <$> vs
    dropLook = makeLookup dropKs
    dropVs = assemble dropKs look <$> vs

dropCols :: Data k => (k -> Bool) -> RFrame k v -> RFrame k v
dropCols p frame = snd (splitCols p frame)

keepCols :: Data k => (k -> Bool) -> RFrame k v -> RFrame k v
keepCols p frame = fst (splitCols p frame)

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
appendRows :: (Data k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
appendRows (RFrame ks0 look0 vs0) (RFrame ks1 look1 vs1) = do
  checkReorder ks0 ks1
  let vs1' = reorder ks0 look1 vs1
  return (RFrame ks0 look0 (vs0 V.++ vs1'))

extendCols :: (Data k, MonadThrow m) => RFrame k v -> RFrame k v -> m (RFrame k v)
extendCols f g = update (toUpdate g) f

takeRows :: Int -> RFrame k v -> RFrame k v
takeRows n (RFrame ks look vs) = RFrame ks look (V.take n vs)
