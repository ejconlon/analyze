module Fixtures where

import           Analyze.CFrame      (CFrame (..))
import           Analyze.RFrame      (RFrame (..))
import           Analyze.Values
import qualified Control.Foldl       as F
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

exampleDecl :: Vector (Text, ValueType)
exampleDecl = V.fromList
  [ ("id", ValueTypeInteger)
  , ("name", ValueTypeText)
  , ("score", ValueTypeDouble)
  ]

exampleHeader :: Vector Text
exampleHeader = V.fromList
  [ "id"
  , "name"
  , "score"
  ]

exampleObj1 :: Vector (Text, Value)
exampleObj1 = V.fromList
  [ ("id", ValueInteger 42)
  , ("name", ValueText "foo")
  , ("score", ValueDouble 5.0)
  ]

exampleRecord1 :: Vector Value
exampleRecord1 = V.fromList
  [ ValueInteger 42
  , ValueText "foo"
  , ValueDouble 50.0
  ]

exampleObj2 :: Vector (Text, Value)
exampleObj2 = V.fromList
  [ ("id", ValueInteger 43)
  , ("name", ValueText "bar")
  , ("score", ValueDouble 3.0)
  ]

exampleRFrame :: RFrame Text Value
exampleRFrame = RFrame names values
  where
    names = V.fromList ["id", "name", "score"]
    values = V.fromList
      [ V.fromList [ValueInteger 42, ValueText "foo", ValueDouble 5.0]
      , V.fromList [ValueInteger 43, ValueText "bar", ValueDouble 3.0]
      ]

exampleCFrame :: CFrame Text Value
exampleCFrame = CFrame names rows cols
  where
    names = V.fromList ["id", "name", "score"]
    rows = 2
    cols = HM.fromList
      [ ("id", V.fromList [ValueInteger 42, ValueInteger 43])
      , ("name", V.fromList [ValueText "foo", ValueText "bar"])
      , ("score", V.fromList [ValueDouble 5.0, ValueDouble 3.0])
      ]

exampleCsv :: Text
exampleCsv = "id,name,score\n" `mappend` "42,foo,5.0\n" `mappend` "43,bar,3.0\n"
