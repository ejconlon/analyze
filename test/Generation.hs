module Generation where

import Analyze.Common (Data)
import Analyze.DSL (DuplicateKeyError(..))
import Analyze.RFrame (RFrame(..))
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Test.QuickCheck

checkForDupes :: (Data k, MonadThrow m) => Vector k -> m ()
checkForDupes vs = go HS.empty (V.toList vs)
  where
    go _ [] = pure ()
    go s (k:ks) =
      if HS.member k s
        then throwM (DuplicateKeyError k)
        else go (HS.insert k s) ks

rawGenerateRFrameSized :: Data k => (t -> Gen v) -> Vector (k, t) -> Int -> Gen (RFrame k v)
rawGenerateRFrameSized prod decl numRows = gen
  where
    rowGen = sequenceA (prod . snd <$> decl)
    allRowsGen = V.replicateM numRows rowGen
    gen = RFrame (fst <$> decl) <$> allRowsGen

generateRFrameSized :: (Data k, MonadThrow m) => (t -> Gen v) -> Vector (k, t) -> Int -> m (Gen (RFrame k v))
generateRFrameSized prod decl numRows = checkForDupes (fst <$> decl) >> pure gen
  where
    gen = rawGenerateRFrameSized prod decl numRows

generateRFrame :: (Data k, MonadThrow m) => (t -> Gen v) -> Vector (k, t) -> m (Gen (RFrame k v))
generateRFrame prod decl = checkForDupes (fst <$> decl) >> pure gen
  where
    gen = sized $ \n -> do
      m <- choose (0, n)
      rawGenerateRFrameSized prod decl m
