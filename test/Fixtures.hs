module Fixtures where

import Analyze.CFrame (CFrame(..))
import Analyze.RFrame (RFrame(..))
import Analyze.Values
import qualified Control.Foldl as F
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector (Vector)

exampleObj :: Vector (Text, Value)
exampleObj = V.fromList
  [ ("id", ValueInteger 42)
  , ("name", ValueText "foo")
  ]

exampleRecord :: Vector Value
exampleRecord = V.fromList
  [ ValueInteger 42
  , ValueText "foo"
  ]

exampleHeader :: Vector Text
exampleHeader = V.fromList
  [ "id"
  , "name"
  ]

exampleDecl :: [(Text, ValueType)]
exampleDecl =
  [ ("id", ValueTypeInteger)
  , ("name", ValueTypeText)
  ]

exampleObj2 :: Vector (Text, Value)
exampleObj2 = V.fromList
  [ ("id", ValueInteger 43)
  , ("name", ValueText "bar")
  ]

exampleRFrame :: RFrame Text Value
exampleRFrame = RFrame names values
  where
    names = V.fromList ["id", "name"]
    values = V.fromList
      [ V.fromList [ValueInteger 42, ValueText "foo"]
      , V.fromList [ValueInteger 43, ValueText "bar"]
      ]

exampleCFrame :: CFrame Text Value
exampleCFrame = CFrame names rows cols
  where
    names = V.fromList ["id", "name"]
    rows = 2
    cols = HM.fromList
      [ ("id", V.fromList [ValueInteger 42, ValueInteger 43])
      , ("name", V.fromList [ValueText "foo", ValueText "bar"])
      ]

exampleCsv :: Text
exampleCsv = "id,name\n" `mappend` "42,foo\n" `mappend` "43,bar\n"
