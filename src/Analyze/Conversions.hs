module Analyze.Conversions
  ( packPFrame
  , unpackPFrame
  , projectRow
  , projectRows
  ) where

import           Analyze.Common                    (Data, MissingKeyError (..), makeLookup)
import           Analyze.PFrame                    (PFrame (..))
import           Analyze.RFrame                    (RFrame (..), RFrameUpdate (..), rframeFromUpdate)
import           Control.Monad.Catch               (MonadThrow (..))
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as VSM
import qualified Pipes                             as P

-- Conversions

packPFrame :: Monad m => RFrame k v -> PFrame m k v
packPFrame (RFrame ks look vs) = PFrame ks look (P.each vs)

-- PFrames can be large, so beware
-- This is generally not what you want to use since it will block
-- until is reads everything into memory.
unpackPFrame :: Monad m => PFrame m k v -> m (RFrame k v)
unpackPFrame (PFrame ks look vp) = result
  where
    unfolded =
      flip VSM.unfoldrM vp $ \p -> do
        n <- P.next p
        return $ case n of
          Left ()       -> Nothing
          Right (a, p') -> Just (a, p')
    result = (\vl -> RFrame ks look (V.fromList vl)) <$> VSM.toList unfolded

projectRow :: (Data k, MonadThrow m) => Vector k -> HashMap k v -> m (Vector v)
projectRow ks row = V.mapM f ks
  where
    f k =
      case HM.lookup k row of
        Nothing -> throwM (MissingKeyError k)
        Just v  -> pure v

-- kind of the inverse of rframeIter
projectRows :: (Data k, MonadThrow m) => Vector k -> Vector (HashMap k v) -> m (RFrame k v)
projectRows ks rs = do
  vs <- V.mapM (projectRow ks) rs
  rframeFromUpdate (RFrameUpdate ks vs)
