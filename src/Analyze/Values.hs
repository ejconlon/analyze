module Analyze.Values where

import           Analyze.Common      (Data)
import           Control.Monad.Catch (Exception, MonadThrow (..))
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)

data ValueType =
    ValueTypeText
  | ValueTypeInteger
  | ValueTypeDouble
  | ValueTypeBool
  deriving (Show, Eq, Enum, Bounded)

data Value =
    ValueText Text
  | ValueInteger Integer
  | ValueDouble Double
  | ValueBool Bool
  deriving (Show, Eq)

valueToType :: Value -> ValueType
valueToType (ValueText _)    = ValueTypeText
valueToType (ValueInteger _) = ValueTypeInteger
valueToType (ValueDouble _)  = ValueTypeDouble
valueToType (ValueBool _)    = ValueTypeBool

getText :: Value -> Maybe Text
getText (ValueText s) = Just s
getText _             = Nothing

getInteger :: Value -> Maybe Integer
getInteger (ValueInteger i) = Just i
getInteger _                = Nothing

getDouble :: Value -> Maybe Double
getDouble (ValueDouble d) = Just d
getDouble _               = Nothing

getBool :: Value -> Maybe Bool
getBool (ValueBool b) = Just b
getBool _             = Nothing

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

boolean :: (Data k, MonadThrow m) => k -> Value -> m Bool
boolean _ (ValueBool s) = pure s
boolean k v             = throwM (ValueTypeError k ValueTypeBool v)
