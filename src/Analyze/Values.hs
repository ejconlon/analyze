module Analyze.Values where

import           Data.Text (Text)

data ValueType =
    ValueTypeText
  | ValueTypeInteger
  | ValueTypeDouble
  deriving (Show, Eq, Enum, Bounded)

data Value =
    ValueText Text
  | ValueInteger Integer
  | ValueDouble Double
  deriving (Show, Eq)

valueToType :: Value -> ValueType
valueToType (ValueText _)    = ValueTypeText
valueToType (ValueInteger _) = ValueTypeInteger
valueToType (ValueDouble _)  = ValueTypeDouble

getText :: Value -> Maybe Text
getText (ValueText s) = Just s
getText _             = Nothing

getInteger :: Value -> Maybe Integer
getInteger (ValueInteger i) = Just i
getInteger _                = Nothing

getDouble :: Value -> Maybe Double
getDouble (ValueDouble d) = Just d
getDouble _               = Nothing
