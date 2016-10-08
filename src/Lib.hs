{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Control.Monad (MonadPlus(..))
import qualified Data.Aeson as A
import qualified Data.Csv as C
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector (Vector)

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

class Indexed r k where
  hasCol :: k -> r k v -> Bool

  mapIndex :: (k -> k) -> r k v -> r k v
  mapIndex fn = runIdentity . mapIndexM (Identity . fn)

  mapIndexM :: Applicative m => (k -> m k) -> r k v -> m (r k v)

  foldIndex :: F.Fold k a -> r k v -> a
  foldIndex ff = runIdentity . foldIndexM (F.generalize ff)

  foldIndexM :: Monad m => F.FoldM m k a -> r k v -> m a

  filterCol :: (k -> Bool) -> r k v -> r k v
  filterCol fn = runIdentity . filterColM (Identity . fn)

  filterColM :: Monad m => (k -> m Bool) -> r k v -> m (r k v)

  mapCol :: k -> (v -> v) -> r k v -> r k v
  mapCol name fn = runIdentity . mapColM name (Identity . fn)

  mapColM :: Applicative m => k -> (v -> m v) -> r k v -> m (r k v)

  mapWithIndex :: (k -> v -> v) -> r k v -> r k v
  mapWithIndex fn = runIdentity . mapWithIndexM (\k v -> Identity (fn k v))

  mapWithIndexM :: Applicative m => (k -> v -> m v) -> r k v -> m (r k v)

  foldCol :: k -> F.Fold v a -> r k v -> a
  foldCol name ff = runIdentity . foldColM name (F.generalize ff)

  foldColM :: Monad m => k -> F.FoldM m v a -> r k v -> m a

  foldWithIndex :: F.Fold (k, v) a -> r k v -> a
  foldWithIndex ff = runIdentity . foldWithIndexM (F.generalize ff)

  foldWithIndexM :: Monad m => F.FoldM m (k, v) a -> r k v -> m a

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

exampleColMaj :: Lookup Text [Value]
exampleColMaj = Lookup
  [ ("id", [ValueInteger 42, ValueInteger 43])
  , ("name", [ValueText "foo", ValueText "bar"])
  ]

data Frame t k v = Frame (Vector k) (t (Vector v)) deriving (Functor, Foldable, Traversable)

instance (Eq k) => Indexed (Frame t) k where
  hasCol name (Frame ks _) = elem name ks
  mapIndexM f (Frame ks vs) =
    (\ks' -> Frame ks' vs) <$> traverse f ks
  foldIndexM ff (Frame ks _) = F.foldM ff ks
  filterColM p m = undefined
  mapColM name fn m = undefined
  mapWithIndexM f m = undefined
  foldColM name ff m = undefined
  foldWithIndexM ff m = undefined

 --  filterColM :: Monad m => (k -> m Bool) -> r k v -> m (r k v)
 --  mapColM :: Applicative m => k -> (v -> m v) -> r k v -> m (r k v)
 --  mapWithIndexM :: Applicative m => (k -> v -> m v) -> r k v -> m (r k v)
 --  foldColM :: Monad m => k -> F.FoldM m v a -> r k v -> m a
 --  foldWithIndexM :: Monad m => F.FoldM m (k, v) a -> r k v -> m a

mfilterM :: (MonadPlus t, Monad m) => Monad m => (a -> m Bool) -> t a -> m (t a)
mfilterM p ta = undefined

class (Indexed f k, Indexed r k) => Rowed f r k where
  foldRow :: F.Fold (r k v) a -> f k v -> a
  foldRow ff = runIdentity . foldRowM (F.generalize ff)
  foldRowM :: Monad m => F.FoldM m (r k v) a -> f k v -> m a
  filterRow :: (r k v -> Bool) -> f k v -> f k v
  filterRow p = runIdentity . filterRowM (Identity . p)
  filterRowM :: Monad m => (r k v -> m Bool) -> f k v -> m (f k v)

lookups :: Functor t => Frame t k v -> t (Lookup k v)
lookups (Frame ks vs) = Lookup . V.toList . V.zip ks <$> vs

instance (MonadPlus t, Traversable t, Eq k) => Rowed (Frame t) Lookup k where
  foldRowM ff = F.foldM ff . lookups
  filterRowM p (Frame ks vs) = (\vs' -> Frame ks vs') <$> mfilterM p' vs
    where p' = p . Lookup . V.toList . V.zip ks

exampleFrame :: Frame [] Text Value
exampleFrame = Frame names values
  where
    names = V.fromList ["id", "name"]
    values =
      [ V.fromList [ValueInteger 42, ValueText "foo"]
      , V.fromList [ValueInteger 43, ValueText "bar"]
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

maxId :: F.Fold Value (Maybe Integer)
maxId = filterFold getInteger F.maximum

exampleMaxId :: Maybe Integer
exampleMaxId = foldCol "id" maxId exampleFrame

exampleCsv :: Text
exampleCsv = "id,name\n" `mappend` "42,foo\n" `mappend` "43,bar\n"

instance A.ToJSON v => A.ToJSON (Lookup Text v) where
  toJSON (Lookup vs) = A.object ((A.toJSON <$>) <$> vs)

instance (Traversable t, A.ToJSON v) => A.ToJSON (Frame t Text v) where
  toJSON frame = A.Array (V.fromList (toList (A.toJSON <$> lookups frame)))

-- instance C.ToField v => C.ToNamedRecord (Lookup Text v) where
--   toNamedRecord (Lookup vs) = undefined

-- instance C.FromField v => C.FromNamedRecord (Lookup Text v) where
--   parseNamedRecord = undefined

-- data FrameError = FrameError deriving (Eq, Show, Typeable)
-- instance Exception FrameError

-- class Dual c where
--   data DualIndex c :: *

-- class (Indexed r, Indexed c) => Framed f r c where
