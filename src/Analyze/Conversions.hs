module Analyze.Conversions where

import           Analyze.CFrame                    (CFrame (..))
import           Analyze.Common                    (Data)
import           Analyze.DSL                       (MissingKeyError (..))
import           Analyze.PFrame                    (PFrame (..))
import           Analyze.RFrame                    (RFrame (..))
import           Control.Monad.Catch               (MonadThrow (..))
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as VSM
import qualified Pipes                             as P

-- Conversions

pframePack :: Monad m => RFrame k v -> PFrame m k v
pframePack (RFrame ks vs) = PFrame ks (P.each vs)

-- PFrames can be large, so beware
-- This is generally not what you want to use since it will block
-- until is reads everything into memory.
pframeUnpack :: Monad m => PFrame m k v -> m (RFrame k v)
pframeUnpack (PFrame ks vp) = result
  where
    unfolded =
      flip VSM.unfoldrM vp $ \p -> do
        n <- P.next p
        return $ case n of
          Left ()       -> Nothing
          Right (a, p') -> Just (a, p')
    result = (\vl -> RFrame ks (V.fromList vl)) <$> VSM.toList unfolded

project :: (Data k, MonadThrow m) => Vector k -> HashMap k v -> m (Vector v)
project ks row = V.mapM f ks
  where
    f k =
      case HM.lookup k row of
        Nothing -> throwM (MissingKeyError k)
        Just v  -> pure v

-- kind of the inverse of rframeIter
projectRow :: (Data k, MonadThrow m) => Vector k -> Vector (HashMap k v) -> m (RFrame k v)
projectRow ks rs = (\vs -> RFrame ks vs) <$> V.mapM (project ks) rs

rowToCol :: Data k => RFrame k v -> CFrame k v
rowToCol (RFrame ks vs) = CFrame ks (V.length vs) dat
  where
    dat = HM.fromList (V.toList (select <$> V.indexed ks))
    select (i, k) = (k, (V.!i) <$> vs)

colToRow :: Data k => CFrame k v -> RFrame k v
colToRow (CFrame ks rs dat) = RFrame ks vs
  where
    vs = V.generate rs f
    f i = (\k -> (dat HM.! k) V.! i) <$> ks
