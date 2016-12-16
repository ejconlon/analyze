{-# LANGUAGE ConstraintKinds #-}

-- Common internal things (no other internal deps)

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

data ColSizeMismatch = ColSizeMismatch Int Int deriving (Show, Eq, Typeable)
instance Exception ColSizeMismatch

data RowSizeMismatch = RowSizeMismatch Int Int deriving (Show, Eq, Typeable)
instance Exception RowSizeMismatch

checkForDupes :: (Data k, MonadThrow m) => Vector k -> m ()
checkForDupes vs = go HS.empty (V.toList vs)
  where
    go _ [] = pure ()
    go s (k:ks) =
      if HS.member k s
        then throwM (DuplicateKeyError k)
        else go (HS.insert k s) ks

checkReorder :: (Data k, MonadThrow m) => Vector k -> Vector k -> m ()
checkReorder xs ys =
  let xSize = V.length xs
      ySize = V.length ys
  in if xSize /= ySize
    then throwM (ColSizeMismatch xSize ySize)
    else checkSubset (V.toList xs) (HS.fromList (V.toList ys))

checkSubset :: (Data k, MonadThrow m) => [k] -> HashSet k -> m ()
checkSubset qs ks = forM_ qs (\q -> unless (HS.member q ks) (throwM (MissingKeyError q)))

makeLookup :: Data k => Vector k -> HashMap k Int
makeLookup = HM.fromList . flip zip [0..] . V.toList 

runLookup :: (Data k, MonadThrow m) => HashMap k Int -> Vector v -> k -> m v
runLookup look vs k =
  case HM.lookup k look >>= (vs V.!?) of
    Nothing -> throwM (MissingKeyError k)
    Just v -> pure v

assemble :: Data k => Vector k -> HashMap k Int -> Vector v -> Vector v
assemble ks look vs = pick <$> ks
  where
    pick k = vs V.! (look HM.! k)

mergeKeys :: Data k => Vector k -> Vector k -> Vector (k, Int, Int)
mergeKeys xs ys =
  let m = HM.fromList (V.toList (V.imap (\i x -> (x, (0, i))) xs))
      n = HM.fromList (V.toList (V.imap (\i x -> (x, (1, i))) ys))
      -- Ties go to the first argument, in this case favoring the update
      o = HM.union n m
      p = (\x -> let (a, b) = o HM.! x in (x, a, b)) <$> xs
      q = (\x -> let (a, b) = n HM.! x in (x, a, b)) <$> (V.filter (\x -> not (HM.member x m)) ys)
  in p V.++ q

runIndexedLookup :: Vector (k, Int, Int) -> Vector v -> Vector v -> Vector v
runIndexedLookup ks xs ys = (\(k, i, j) -> (if i == 0 then xs else ys) V.! j) <$> ks

reorder :: Data k => Vector k -> HashMap k Int -> Vector v -> Vector v
reorder ks look vs = (\k -> vs V.! (look HM.! k)) <$> ks
