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
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as VSM
import Data.Vector (Vector)
import Pipes as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Preamble

-- lookupLookup :: Eq a => a -> Lookup a b -> Maybe b
-- lookupLookup a (Lookup xs) = lookup a xs

-- lookupHas :: Eq a => a -> [(a, b)] -> Bool
-- lookupHas _ [] = False
-- lookupHas name ((n, _):xs) =
--   if name == n
--     then True
--     else lookupHas name xs

-- lookupMap :: Eq a => a -> (b -> b) -> [(a, b)] -> [(a, b)]
-- lookupMap _ _ [] = []
-- lookupMap name fn ((n, v):xs) =
--   if name == n
--     then (n, (fn v)) : xs
--     else lookupMap name fn xs
    
-- lookupMapF :: (Applicative f, Eq a) => a -> (b -> f b) -> [(a, b)] -> f [(a, b)]
-- lookupMapF _ _ [] = pure []
-- lookupMapF name fn ((n, v):xs) =
--   if name == n
--     then (\v' -> (n, v') : xs) <$> (fn v)
--     else lookupMapF name fn xs

-- lookupMapWithIndex :: (a -> b -> b) -> [(a, b)] -> [(a, b)]
-- lookupMapWithIndex _ [] = []
-- lookupMapWithIndex fn ((n, v):xs) = (n, (fn n v)) : (lookupMapWithIndex fn xs)

-- lookupMapWithIndexF :: Applicative f => (a -> b -> f b) -> [(a, b)] -> f [(a, b)]
-- lookupMapWithIndexF f = traverse (\(n, v) -> ((\v' -> (n, v')) <$> f n v))
    
-- lookupImap :: (a -> a) -> [(a, b)] -> [(a, b)]
-- lookupImap _ [] = []
-- lookupImap f ((n, v):xs) = (f n, v) : xs

-- lookupImapF :: Applicative f => (a -> f a) -> [(a, b)] -> f [(a, b)]
-- lookupImapF f xs = traverse (\(n, v) -> (\n' -> (n', v)) <$> f n) xs
    
-- lookupFilter :: (a -> Bool) -> [(a, b)] -> [(a, b)]
-- lookupFilter f = filter (\(a, _) -> f a)

-- lookupFilterF :: Monad f => (a -> f Bool) -> [(a, b)] -> f [(a, b)]
-- lookupFilterF f [] = pure []
-- lookupFilterF f (x@(k, _):xs) = do
--   v <- f k
--   w <- lookupFilterF f xs
--   return $ if v then (x:w) else w

-- Types

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

-- newtype Lookup k v = Lookup [(k, v)]
--   deriving (Show, Eq, Monoid, Functor, Foldable, Traversable)

-- class Indexed r k where
--   hasCol :: k -> r k v -> Bool

--   mapIndex :: (k -> k) -> r k v -> r k v
--   mapIndex fn = runIdentity . mapIndexM (Identity . fn)

--   mapIndexM :: Applicative m => (k -> m k) -> r k v -> m (r k v)

--   foldIndex :: F.Fold k a -> r k v -> a
--   foldIndex ff = runIdentity . foldIndexM (F.generalize ff)

--   foldIndexM :: Monad m => F.FoldM m k a -> r k v -> m a

--   filterCol :: (k -> Bool) -> r k v -> r k v
--   filterCol fn = runIdentity . filterColM (Identity . fn)

--   filterColM :: Monad m => (k -> m Bool) -> r k v -> m (r k v)

--   mapCol :: k -> (v -> v) -> r k v -> r k v
--   mapCol name fn = runIdentity . mapColM name (Identity . fn)

--   mapColM :: Applicative m => k -> (v -> m v) -> r k v -> m (r k v)

--   mapWithIndex :: (k -> v -> v) -> r k v -> r k v
--   mapWithIndex fn = runIdentity . mapWithIndexM (\k v -> Identity (fn k v))

--   mapWithIndexM :: Applicative m => (k -> v -> m v) -> r k v -> m (r k v)

--   foldCol :: k -> F.Fold v a -> r k v -> a
--   foldCol name ff = runIdentity . foldColM name (F.generalize ff)

--   foldColM :: Monad m => k -> F.FoldM m v a -> r k v -> m a

--   foldWithIndex :: F.Fold (k, v) a -> r k v -> a
--   foldWithIndex ff = runIdentity . foldWithIndexM (F.generalize ff)

--   foldWithIndexM :: Monad m => F.FoldM m (k, v) a -> r k v -> m a

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

exampleObj :: HashMap Text Value
exampleObj = HM.fromList
  [ ("id", ValueInteger 42)
  , ("name", ValueText "foo")
  ]

exampleRecord :: [Value]
exampleRecord =
  [ ValueInteger 42
  , ValueText "foo"
  ]

exampleHeader :: [Text]
exampleHeader =
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

data FrameError k vt v =
    MissingKeyError k
  | ValueTypeError k vt v
  deriving (Show, Eq)

data Arg k vt a = Arg k vt deriving (Eq, Show)

type Handler k vt a = Ap (Arg k vt) a

handlerTypes :: Handler k vt a -> [(k, vt)]
handlerTypes = undefined

-- type Trans f g = forall x. f x -> g x

-- runHandler :: Monad m => Handler k vt a -> Trans (Arg k vt) m -> HashMap k v -> m a
-- runHandler = undefined

argText :: k -> Arg k ValueType Text
argText k = Arg k ValueTypeText

argInteger :: k -> Arg k ValueType Integer
argInteger k = Arg k ValueTypeInteger

argDouble :: k -> Arg k ValueType Double
argDouble k = Arg k ValueTypeDouble

-- In-memory row-oriented frame
data RFrame k v = RFrame
  { rframeKeys :: !(Vector k)
  , rframeValues :: !(Vector (Vector v))
  } deriving (Functor, Foldable, Traversable)

rframeCols :: RFrame k v -> Int
rframeCols (RFrame ks _) = V.length ks

rframeRows :: RFrame k v -> Int
rframeRows (RFrame _ vs) = V.length vs

-- In-memory col-oriented frame
data CFrame k v = CFrame
  { cframeKeys :: !(Vector k)
  , cframeRows :: !Int
  , cframeMap :: !(HashMap k (Vector v))
  } deriving (Functor, Foldable, Traversable)

cframeCols :: CFrame k v -> Int
cframeCols (CFrame ks _ _) = V.length ks

-- Streaming row-oriented frame
data PFrame m k v = PFrame
  { pframeKeys :: !(Vector k)
  , pframeValues :: !(P.Producer (Vector v) m ())
  }

instance Monad m => Functor (PFrame m k) where
  fmap f (PFrame ks vs) = PFrame ks (P.for vs (P.yield . fmap f))

pframeCols :: PFrame m k v -> Int
pframeCols (PFrame ks _) = V.length ks

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

rframeIter :: (Eq k, Hashable k) => RFrame k v -> Vector (HashMap k v)
rframeIter (RFrame ks vs) = HM.fromList . V.toList . V.zip ks <$> vs

rowToCol :: RFrame k v -> CFrame k v
rowToCol = undefined

colToRow :: CFrame k v -> RFrame k v
colToRow = undefined

-- Re-evaluates value rows again and again for each key
-- So beware...
--frameIterCols :: Frame k v -> HashMap k (Vector v)
--frameIterCols = undefined
-- frameCols (Frame ks vs) = Lookup (zipWith f [(0 :: Int)..] (V.toList ks))
--   where
--     f i k = (k, ((V.! i) <$> vs))

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

filterFold :: (v -> Maybe w) -> F.Fold w z -> F.Fold v z
filterFold e (F.Fold step begin done) = F.Fold step' begin done
  where
    step' a v =
      case e v of
        Nothing -> a
        Just w -> step a w

filterFoldM :: Applicative m => (v -> Maybe w) -> F.FoldM m w z -> F.FoldM m v z
filterFoldM e (F.FoldM step begin done) = F.FoldM step' begin done
  where
    step' a v =
      case e v of
        Nothing -> pure a
        Just w -> step a w

-- maxId :: F.Fold (Lookup Text Value) (Maybe Integer)
-- maxId = filterFold (lookupLookup "id" >=> getInteger) F.maximum

-- exampleMaxId :: Maybe Integer
-- exampleMaxId = runIdentity (foldRow maxId exampleFrame)

exampleCsv :: Text
exampleCsv = "id,name\n" `mappend` "42,foo\n" `mappend` "43,bar\n"

-- instance A.ToJSON v => A.ToJSON (HashMap Text v) where
--   toJSON (Lookup vs) = A.object ((A.toJSON <$>) <$> vs)

instance A.ToJSON v => A.ToJSON (RFrame Text v) where
  toJSON frame = A.Array (A.toJSON <$> rframeIter frame)

-- instance C.ToField v => C.ToNamedRecord (Lookup Text v) where
--   toNamedRecord (Lookup vs) = undefined

-- instance C.FromField v => C.FromNamedRecord (Lookup Text v) where
--   parseNamedRecord = undefined

-- data FrameError = FrameError deriving (Eq, Show, Typeable)
-- instance Exception FrameError
