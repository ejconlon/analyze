module Analyze.PFrame where

import Analyze.Common (Data)
import Analyze.Decoding (Decoder(..), decodeRow)
import qualified Control.Foldl                     as F
import           Control.Monad.Catch               (MonadThrow(..))
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V
import qualified Pipes as P

-- Streaming row-oriented frame
data PFrame m k v = PFrame
  { pframeKeys :: !(Vector k)
  , pframeData :: !(P.Producer (Vector v) m ())
  }

instance Monad m => Functor (PFrame m k) where
  fmap f (PFrame ks vs) = PFrame ks (P.for vs (P.yield . fmap f))

pframeCols :: PFrame m k v -> Int
pframeCols (PFrame ks _) = V.length ks

pframeFold :: Monad m => F.FoldM m (Vector v) a -> PFrame m k v -> m a
pframeFold = undefined

pframeFoldKV :: Monad m => F.FoldM m (Vector (k, v)) a -> PFrame m k v -> m a
pframeFoldKV = undefined

pframeDecode :: MonadThrow m => Decoder m k v a -> PFrame m k v -> P.Producer a m ()
pframeDecode = undefined
