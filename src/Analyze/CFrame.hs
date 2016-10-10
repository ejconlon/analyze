{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Analyze.CFrame where

import           Analyze.Common      (Data)
import           Control.Monad.Catch (MonadThrow (..))
import qualified Data.Aeson          as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

-- In-memory col-oriented frame
data CFrame k v = CFrame
  { cframeKeys :: !(Vector k)
  , cframeRows :: !Int
  , cframeData :: !(HashMap k (Vector v))
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- TODO Just write all fields as is
instance A.ToJSON v => A.ToJSON (CFrame Text v) where
  toJSON frame = undefined

cframeCols :: CFrame k v -> Int
cframeCols (CFrame ks _ _) = V.length ks

-- Merge two CFrames with the given col merge function if overlapping
-- Retains the order of columns as seen in the first then second (minus repeats)
-- Will throw on row length mismatch
cframeMerge :: MonadThrow m => (k -> v -> v -> v) -> CFrame k v -> CFrame k v -> m (CFrame k v)
cframeMerge = undefined

-- TODO add index to predicate
cframeFilter :: (k -> Bool) -> CFrame k v -> CFrame k v
cframeFilter p (CFrame ks rs cs) = CFrame ks' rs cs'
  where
    ks' = V.filter p ks
    p' k _ = p k
    cs' = HM.filterWithKey p' cs
