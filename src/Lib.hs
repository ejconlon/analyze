{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative.Free
import qualified Control.Foldl as F
import Control.Monad ((>=>))
import Control.Monad.Catch
import qualified Data.Aeson as A
import qualified Data.Csv as C
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as VSM
import Data.Vector (Vector)
import Pipes as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Values

data ValueType =
    ValueTypeText
  | ValueTypeInteger
  | ValueTypeDouble
  deriving (Show, Eq)

data Value =
    ValueText Text
  | ValueInteger Integer
  | ValueDouble Double
  deriving (Show, Eq)

valueToType :: Value -> ValueType
valueToType (ValueText _) = ValueTypeText
valueToType (ValueInteger _) = ValueTypeInteger
valueToType (ValueDouble _) = ValueTypeDouble

getText :: Value -> Maybe Text
getText (ValueText s) = Just s
getText _ = Nothing

getInteger :: Value -> Maybe Integer
getInteger (ValueInteger i) = Just i
getInteger _ = Nothing

getDouble :: Value -> Maybe Double
getDouble (ValueDouble d) = Just d
getDouble _ = Nothing

-- Decoding

data DArg m k v a = DArg k (v -> m a) deriving (Functor)

type Decoder m k v a = Ap (DArg m k v) a

decoderKeys :: Decoder m k v a -> [k]
decoderKeys = undefined

data MissingKeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (MissingKeyError k)

runDecoder :: MonadThrow m => Decoder m k v a -> HashMap k v -> m a
runDecoder = undefined

-- Decoding Values

data ValueTypeError k = ValueTypeError k ValueType Value deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (ValueTypeError k)

textOrError :: (Show k, Typeable k, MonadThrow m) => k -> Value -> m Text
textOrError _ (ValueText s) = pure s
textOrError k v = throwM (ValueTypeError k ValueTypeText v)

argText :: (Show k, Typeable k, MonadThrow m) => k -> DArg m k Value Text
argText k = DArg k (textOrError k)

-- RFrame

-- In-memory row-oriented frame
data RFrame k v = RFrame
  { rframeKeys :: !(Vector k)
  , rframeData :: !(Vector (Vector v))
  } deriving (Functor, Foldable, Traversable)

instance A.ToJSON v => A.ToJSON (RFrame Text v) where
  toJSON frame = A.Array (A.toJSON <$> rframeIter frame)

rframeCols :: RFrame k v -> Int
rframeCols (RFrame ks _) = V.length ks

rframeRows :: RFrame k v -> Int
rframeRows (RFrame _ vs) = V.length vs

rframeIter :: (Eq k, Hashable k) => RFrame k v -> Vector (HashMap k v)
rframeIter (RFrame ks vs) = HM.fromList . V.toList . V.zip ks <$> vs

rframeDecode :: (Hashable k, Eq k, MonadThrow m) => Decoder m k v a -> RFrame k v -> Vector (m a)
rframeDecode decoder rframe = runDecoder decoder <$> rframeIter rframe

rframeFilter :: (k -> Bool) -> RFrame k v -> RFrame k v
rframeFilter = undefined

-- Will throw on col missing
rframeGetCol :: MonadThrow m => k -> RFrame k v -> m (Vector v)
rframeGetCol = undefined

-- Will append if not present
-- Will throw on row length mismatch
rframeSetCol :: MonadThrow m => k -> Vector v -> RFrame k v -> m (RFrame k v)
rframeSetCol = undefined

-- Will throw on col mismatch
rframeAddRow :: MonadThrow m => HashMap k v -> RFrame k v -> m (RFrame k v)
rframeAddRow = undefined

-- Appends row-wise, retaining column order of the first
-- Will throw on col mismatch
rframeAppend :: MonadThrow m => RFrame k v -> RFrame k v -> m (RFrame k v)
rframeAppend = undefined

-- CFrame

-- In-memory col-oriented frame
data CFrame k v = CFrame
  { cframeKeys :: !(Vector k)
  , cframeRows :: !Int
  , cframeData :: !(HashMap k (Vector v))
  } deriving (Functor, Foldable, Traversable)

cframeCols :: CFrame k v -> Int
cframeCols (CFrame ks _ _) = V.length ks

cframeDecode :: MonadThrow m => Decoder m k v a -> CFrame k v -> Vector (m a)
cframeDecode = undefined

-- Will throw on out of bounds
cframeGetRow :: MonadThrow m => Int -> CFrame k v -> m (HashMap k v)
cframeGetRow = undefined

-- Will throw on out of bounds or col mismatch
cframeSetRow :: MonadThrow m => Int -> HashMap k v -> CFrame k v -> m (CFrame k v)
cframeSetRow = undefined

-- Will throw on col mismatch
cframeAddRow :: MonadThrow m => HashMap k v -> CFrame k v -> m (CFrame k v)
cframeAddRow = undefined

-- Merge two CFrames with the given col merge function if overlapping
-- Retains the order of columns as seen in the first then second (minus repeats)
-- Will throw on row length mismatch
cframeMerge :: MonadThrow m => (k -> v -> v -> v) -> CFrame k v -> CFrame k v -> m (CFrame k v)
cframeMerge = undefined

-- PFrame

-- Streaming row-oriented frame
data PFrame m k v = PFrame
  { pframeKeys :: !(Vector k)
  , pframeData :: !(P.Producer (Vector v) m ())
  }

instance Monad m => Functor (PFrame m k) where
  fmap f (PFrame ks vs) = PFrame ks (P.for vs (P.yield . fmap f))

pframeCols :: PFrame m k v -> Int
pframeCols (PFrame ks _) = V.length ks

pframeDecode :: MonadThrow m => Decoder m k v a -> PFrame m k v -> P.Producer a m ()
pframeDecode = undefined

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
          Left () -> Nothing
          Right (a, p') -> Just (a, p')
    result = (\vl -> RFrame ks (V.fromList vl)) <$> VSM.toList unfolded

rowToCol :: RFrame k v -> CFrame k v
rowToCol = undefined

colToRow :: CFrame k v -> RFrame k v
colToRow = undefined

-- Examples

exampleObj :: HashMap Text Value
exampleObj = HM.fromList
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

exampleObj2 :: HashMap Text Value
exampleObj2 = HM.fromList
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

-- Folding

-- filterFold :: (v -> Maybe w) -> F.Fold w z -> F.Fold v z
-- filterFold e (F.Fold step begin done) = F.Fold step' begin done
--   where
--     step' a v =
--       case e v of
--         Nothing -> a
--         Just w -> step a w

-- filterFoldM :: Applicative m => (v -> Maybe w) -> F.FoldM m w z -> F.FoldM m v z
-- filterFoldM e (F.FoldM step begin done) = F.FoldM step' begin done
--   where
--     step' a v =
--       case e v of
--         Nothing -> pure a
--         Just w -> step a w

-- maxId :: F.Fold (Lookup Text Value) (Maybe Integer)
-- maxId = filterFold (lookupLookup "id" >=> getInteger) F.maximum

-- exampleMaxId :: Maybe Integer
-- exampleMaxId = runIdentity (foldRow maxId exampleFrame)

-- instance A.ToJSON v => A.ToJSON (HashMap Text v) where
--   toJSON (Lookup vs) = A.object ((A.toJSON <$>) <$> vs)

-- instance C.ToField v => C.ToNamedRecord (Lookup Text v) where
--   toNamedRecord (Lookup vs) = undefined

-- instance C.FromField v => C.FromNamedRecord (Lookup Text v) where
--   parseNamedRecord = undefined
