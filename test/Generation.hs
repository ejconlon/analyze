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

generateRFrame :: (Data k, MonadThrow m) => Vector (k, t) -> (t -> Gen v) -> m (Gen (RFrame k v))
generateRFrame decl prod = checkForDupes (fst <$> decl) >> pure gen
  where
    rowGen = sequenceA (prod . snd <$> decl)
    allRowsGen = sized $ \n -> do
      k <- choose (0, n)
      V.replicateM k rowGen
    gen = RFrame (fst <$> decl) <$> allRowsGen
