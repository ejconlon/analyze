{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Control.Foldl as F
import qualified Data.Aeson as A
import qualified Data.Csv as C
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Data.Text (Text)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Preamble

-- TODO these are right folds

lookupHas :: Eq a => a -> [(a, b)] -> Bool
lookupHas _ [] = False
lookupHas name ((n, _):xs) =
  if name == n
    then True
    else lookupHas name xs

lookupMap :: Eq a => a -> (b -> b) -> [(a, b)] -> [(a, b)]
lookupMap _ _ [] = []
lookupMap name fn ((n, v):xs) =
  if name == n
    then (n, (fn v)) : xs
    else lookupMap name fn xs
    
lookupMapF :: (Applicative f, Eq a) => a -> (b -> f b) -> [(a, b)] -> f [(a, b)]
lookupMapF _ _ [] = pure []
lookupMapF name fn ((n, v):xs) =
  if name == n
    then (\v' -> (n, v') : xs) <$> (fn v)
    else lookupMapF name fn xs

lookupMapWithIndex :: (a -> b -> b) -> [(a, b)] -> [(a, b)]
lookupMapWithIndex _ [] = []
lookupMapWithIndex fn ((n, v):xs) = (n, (fn n v)) : (lookupMapWithIndex fn xs)

lookupMapWithIndexF :: Applicative f => (a -> b -> f b) -> [(a, b)] -> f [(a, b)]
lookupMapWithIndexF f = traverse (\(n, v) -> ((\v' -> (n, v')) <$> f n v))
    
lookupImap :: (a -> a) -> [(a, b)] -> [(a, b)]
lookupImap _ [] = []
lookupImap f ((n, v):xs) = (f n, v) : xs

lookupImapF :: Applicative f => (a -> f a) -> [(a, b)] -> f [(a, b)]
lookupImapF f xs = traverse (\(n, v) -> (\n' -> (n', v)) <$> f n) xs
    
lookupFilter :: (a -> Bool) -> [(a, b)] -> [(a, b)]
lookupFilter f = filter (\(a, _) -> f a)

lookupFilterF :: Monad f => (a -> f Bool) -> [(a, b)] -> f [(a, b)]
lookupFilterF f [] = pure []
lookupFilterF f (x@(k, _):xs) = do
  v <- f k
  w <- lookupFilterF f xs
  return $ if v then (x:w) else w

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

newtype Lookup k v = Lookup [(k, v)]
  deriving (Show, Eq, Monoid, Functor, Foldable, Traversable)

class Traversable (c k) => Indexed c k where
  hasCol :: k -> c k v -> Bool

  mapIndex :: (k -> k) -> c k v -> c k v
  mapIndex fn = runIdentity . mapIndexM (Identity . fn)

  mapIndexM :: Applicative m => (k -> m k) -> c k v -> m (c k v)

  foldIndex :: F.Fold k a -> c k v -> a
  foldIndex ff = runIdentity . foldIndexM (F.generalize ff)

  foldIndexM :: Monad m => F.FoldM m k a -> c k v -> m a

  filterCol :: (k -> Bool) -> c k v -> c k v
  filterCol fn = runIdentity . filterColM (Identity . fn)

  filterColM :: Monad m => (k -> m Bool) -> c k v -> m (c k v)

  mapCol :: k -> (v -> v) -> c k v -> c k v
  mapCol name fn = runIdentity . mapColM name (Identity . fn)

  mapColM :: Applicative m => k -> (v -> m v) -> c k v -> m (c k v)

  mapWithIndex :: (k -> v -> v) -> c k v -> c k v
  mapWithIndex fn = runIdentity . mapWithIndexM (\k v -> Identity (fn k v))

  mapWithIndexM :: Applicative m => (k -> v -> m v) -> c k v -> m (c k v)

  foldCol :: k -> F.Fold v a -> c k v -> a
  foldCol name ff = runIdentity . foldColM name (F.generalize ff)

  foldColM :: Monad m => k -> F.FoldM m v a -> c k v -> m a

  foldWithIndex :: F.Fold (k, v) a -> c k v -> a
  foldWithIndex ff = runIdentity . foldWithIndexM (F.generalize ff)

  foldWithIndexM :: Monad m => F.FoldM m (k, v) a -> c k v -> m a

valueToType :: Value -> ValueType
valueToType (ValueText _) = ValueTypeText
valueToType (ValueInteger _) = ValueTypeInteger
valueToType (ValueDouble _) = ValueTypeDouble

bindText :: (Text -> Value) -> Value -> Value
bindText fn (ValueText s) = fn s
bindText _ v = v

bindInteger :: (Integer -> Value) -> Value -> Value
bindInteger fn (ValueInteger i) = fn i
bindInteger _ v = v

bindDouble :: (Double -> Value) -> Value -> Value
bindDouble fn (ValueDouble d) = fn d
bindDouble _ v = v

instance Eq k => Indexed Lookup k where
  hasCol name (Lookup os) = lookupHas name os
  mapIndexM f (Lookup os) = Lookup <$> lookupImapF f os
  foldIndexM ff (Lookup os) = F.foldM ff (fst <$> os)
  filterColM p (Lookup os) = Lookup <$> lookupFilterF p os
  mapColM name fn (Lookup os) = Lookup <$> lookupMapF name fn os
  mapWithIndexM f (Lookup os) = Lookup <$> lookupMapWithIndexF f os
  foldColM name ff (Lookup os) = F.foldM ff (lookup name os)
  foldWithIndexM ff (Lookup os) = F.foldM ff os
  
instance (Eq k, Ord k) => Indexed Map k where
  hasCol name m = undefined
  mapIndexM f m = undefined
  foldIndexM ff m = undefined
  filterColM p m = undefined
  mapColM name fn m = undefined
  mapWithIndexM f m = undefined
  foldColM name ff m = undefined
  foldWithIndexM ff m = undefined

instance (Eq k, Hashable k) => Indexed HashMap k where
  hasCol name m = undefined
  mapIndexM f m = undefined
  foldIndexM ff m = undefined
  filterColM p m = undefined
  mapColM name fn m = undefined
  mapWithIndexM f m = undefined
  foldColM name ff m = undefined
  foldWithIndexM ff m = undefined

exampleObj :: Lookup Text Value
exampleObj = Lookup
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

exampleDecl :: Lookup Text ValueType
exampleDecl = Lookup
  [ ("id", ValueTypeInteger)
  , ("name", ValueTypeText)
  ]

exampleObj2 :: Lookup Text Value
exampleObj2 = Lookup
  [ ("id", ValueInteger 43)
  , ("name", ValueText "bar")
  ]

exampleColMaj :: Lookup Text [(Integer, Value)]
exampleColMaj = Lookup
  [ ("id", [(80, ValueInteger 42), (81, ValueInteger 43)])
  , ("name", [(80, ValueText "foo"), (81, ValueText "bar")])
  ]

-- addCol name values exampleColMaj
-- addRow id value exampleColMaj

exampleRowMaj :: Lookup Integer [(Text, Value)]
exampleRowMaj =
  let (Lookup o1) = exampleObj
      (Lookup o2) = exampleObj2
  in Lookup [ (80, o1), (81, o2) ]

data Frame r c t v = Frame (Lookup c t) (Lookup r v)

exampleFrame :: Frame Integer Text ValueType [Value]
exampleFrame = Frame types values
  where
    types = Lookup
      [ ("id", ValueTypeInteger)
      , ("name", ValueTypeText)
      ]
    values = Lookup
      [ (80, [ValueInteger 42, ValueText "foo"])
      , (81, [ValueInteger 43, ValueText "boo"])
      ]

-- class Rowed f rk c where
--   hasRow :: rk -> f -> Bool

--   --foldRow ::  (RowIndex f, n) m -> f -> m
--   filterRow :: (RowIndex f -> Bool) ->  p m = undefined


--newtype Transpose f = Transpose f deriving (Show, Eq)

-- class (Indexed r, Indexed c, Framey f r c) => Framey (Transpose f) c r

instance A.ToJSON v => A.ToJSON (Lookup Text v) where
  toJSON (Lookup vs) = A.object ((A.toJSON <$>) <$> vs)

instance A.FromJSON v => A.FromJSON (Lookup Text v) where
  parseJSON = undefined

instance C.ToField v => C.ToNamedRecord (Lookup Text v) where
  toNamedRecord (Lookup vs) = undefined

instance C.FromField v => C.FromNamedRecord (Lookup Text v) where
  parseNamedRecord = undefined

-- data FrameError = FrameError deriving (Eq, Show, Typeable)
-- instance Exception FrameError

-- class Dual c where
--   data DualIndex c :: *

-- class (Indexed r, Indexed c) => Framed f r c where
