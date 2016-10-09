{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Analyze.RFrame where

import Analyze.Common (Data)
import Analyze.Decoding (Decoder(..), decodeRow)
import qualified Data.Aeson                        as A
import qualified Control.Foldl                     as F
import           Control.Monad.Catch               (MonadThrow(..))
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
import Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V

-- In-memory row-oriented frame
data RFrame k v = RFrame
  { rframeKeys :: !(Vector k)
  , rframeData :: !(Vector (Vector v))
  } deriving (Functor, Foldable, Traversable)

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

rframeDecode :: (Data k, Monad m) => Decoder m k v a -> RFrame k v -> Vector (m a)
rframeDecode decoder rframe = decodeRow decoder . HM.fromList . V.toList <$> rframeIter rframe

rframeFilter :: (Vector (k, v) -> Bool) -> RFrame k v -> RFrame k v
rframeFilter = undefined

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
rframeAppend :: MonadThrow m => RFrame k v -> RFrame k v -> m (RFrame k v)
rframeAppend = undefined
