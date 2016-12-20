module Analyze.Ops where

import Analyze.Common (Data)
import Analyze.RFrame (RFrame(..))
import Control.Monad.Catch (MonadThrow(..))

oneHot :: Data k => (k -> v -> k) -> k -> v -> v -> RFrame k v -> RFrame k v
oneHot = undefined

-- unHot :: (Data k, MonadThrow m) => (k -> m (Maybe (k, v))) -> k -> RFrame k v -> RFrame k v
-- unHot = undefined
