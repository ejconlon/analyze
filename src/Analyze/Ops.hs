module Analyze.Ops where

import Analyze.Common (Data)
import Analyze.RFrame (RFrame(..))

oneHot :: Data k => (k -> Bool) -> (k -> v -> k) -> v -> v -> RFrame k v -> RFrame k v
oneHot = undefined
