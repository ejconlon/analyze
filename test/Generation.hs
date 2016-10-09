module Generation where

import Analyze.Common (Data)
import qualified Data.Vector as V
import Data.Vector (Vector)
import QuickCheck

generateRFrame :: Data k => Vector (k, t) -> (t -> Gen v) -> Int -> Gen (RFrame k v)
generateRFrame = undefined
