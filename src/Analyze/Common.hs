{-# LANGUAGE ConstraintKinds #-}

module Analyze.Common where

import           Control.Exception
import           Control.Monad.Catch (MonadThrow (..))
import           Control.Monad (forM_, unless)
import           Data.Hashable (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.HashSet (HashSet)
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import           Data.Vector (Vector)

type Data k = (Eq k, Hashable k, Show k, Typeable k)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) x f = f <$> x
{-# INLINE (<&>) #-}
infixl 1 <&>

data MissingKeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (MissingKeyError k)

data DuplicateKeyError k = DuplicateKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (DuplicateKeyError k)

checkForDupes :: (Data k, MonadThrow m) => Vector k -> m ()
checkForDupes vs = go HS.empty (V.toList vs)
  where
    go _ [] = pure ()
    go s (k:ks) =
      if HS.member k s
        then throwM (DuplicateKeyError k)
        else go (HS.insert k s) ks

checkSubset :: (Data k, MonadThrow m) => [k] -> HashSet k -> m ()
checkSubset qs ks = forM_ qs (\q -> unless (HS.member q ks) (throwM (MissingKeyError q)))

lookupOrThrow :: (Data k, MonadThrow m) => k -> HashMap k v -> m v
lookupOrThrow k m =
  case HM.lookup k m of
    Nothing -> throwM (MissingKeyError k)
    Just v -> pure v
