module Analyze.DSL where

import           Analyze.Common      (Data)
import           Analyze.Values
import           Control.Monad.Catch (Exception, MonadThrow (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)

data ValueTypeError k = ValueTypeError k ValueType Value deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (ValueTypeError k)

textual :: (Data k, MonadThrow m) => k -> Value -> m Text
textual _ (ValueText s) = pure s
textual k v             = throwM (ValueTypeError k ValueTypeText v)

integral :: (Data k, MonadThrow m) => k -> Value -> m Integer
integral _ (ValueInteger s) = pure s
integral k v                = throwM (ValueTypeError k ValueTypeInteger v)

floating :: (Data k, MonadThrow m) => k -> Value -> m Double
floating _ (ValueDouble s) = pure s
floating k v               = throwM (ValueTypeError k ValueTypeDouble v)
