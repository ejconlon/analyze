module Analyze.DSL where

import Analyze.Common (Data)
import Analyze.Decoding (Arg(..))
import Analyze.Values
import qualified Control.Foldl as F
import Control.Monad ((>=>))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Typeable (Typeable)
import qualified Data.Text as T
import Data.Text (Text)

data MissingKeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (MissingKeyError k)

data ValueTypeError k = ValueTypeError k ValueType Value deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (ValueTypeError k)

-- analogous to premapM
andThen :: Monad m => F.FoldM m v a -> (a -> m b) -> F.FoldM m v b
andThen (F.FoldM step begin done) f = F.FoldM step begin (done >=> f)

orElse :: Monad m => F.FoldM m v (Maybe a) -> m a -> F.FoldM m v a
orElse f act = f `andThen` act'
  where
    act' Nothing  = act
    act' (Just x) = pure x

require :: (Data k, MonadThrow m) => k -> (k -> v -> m a) -> Arg m k v a
require k e = Arg k (F.generalize F.head `orElse` throwM (MissingKeyError k) `andThen` e k)

textual :: (Data k, MonadThrow m) => k -> Value -> m Text
textual _ (ValueText s) = pure s
textual k v             = throwM (ValueTypeError k ValueTypeText v)

integral :: (Data k, MonadThrow m) => k -> Value -> m Integer
integral _ (ValueInteger s) = pure s
integral k v                = throwM (ValueTypeError k ValueTypeInteger v)

floating :: (Data k, MonadThrow m) => k -> Value -> m Double
floating _ (ValueDouble s) = pure s
floating k v               = throwM (ValueTypeError k ValueTypeDouble v)
