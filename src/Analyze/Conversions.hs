module Analyze.Conversions
  ( projectRow
  , projectRows
  ) where

import           Analyze.Common      (Data, MissingKeyError (..), makeLookup)
import           Analyze.RFrame      (RFrame (..), RFrameUpdate (..), fromUpdate)
import           Control.Monad.Catch (MonadThrow (..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

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
  fromUpdate (RFrameUpdate ks vs)
