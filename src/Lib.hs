{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

import qualified Control.Foldl as F

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
    
lookupDrop :: Eq a => a -> [(a, b)] -> [(a, b)]
lookupDrop _ [] = []
lookupDrop name ((n, v):xs) =
  if name == n
    then xs
    else lookupDrop name xs

-- Types

data ValueType =
    ValueTypeString
  | ValueTypeInteger
  | ValueTypeDouble
  deriving (Show, Eq)

data Value =
    ValueString String
  | ValueInteger Integer
  | ValueDouble Double
  deriving (Show, Eq)

newtype Lookup k v = Lookup [(k, v)]
  deriving (Show, Eq)

class Indexed c where
  type Index c :: *
  type Data c :: *

  hasCol :: Index c -> c -> Bool

  dropCol :: Index c -> c -> c
  
  mapCol :: Index c -> (Data c -> Data c) -> c -> c
  mapColF :: Applicative f => Index c -> (Data c -> f (Data c)) -> c -> f c

  mapWithIndex :: (Index c -> Data c -> Data c) -> c -> c
  mapWithIndexF :: Applicative f => (Index c -> Data c -> f (Data c)) -> c -> f c

  mapIndex :: (Index c -> Index c) -> c -> c
  mapIndexF :: Applicative f => (Index c -> f (Index c)) -> c -> f c

  foldCol :: Index c -> F.Fold (Data c) a -> c -> a
  foldColF :: Monad f => Index c -> F.FoldM f (Data c) a -> c -> f a

  foldWithIndex :: F.Fold (Index c, Data c) a -> c -> a
  foldWithIndexF :: Monad f => F.FoldM f (Index c, Data c) a -> c -> f a

  foldIndex :: F.Fold (Index c) a -> c -> a
  foldIndexF :: Monad f => F.FoldM f (Index c) a -> c -> f a

valueToType :: Value -> ValueType
valueToType (ValueString _) = ValueTypeString
valueToType (ValueInteger _) = ValueTypeInteger
valueToType (ValueDouble _) = ValueTypeDouble

bindString :: (String -> Value) -> Value -> Value
bindString fn (ValueString s) = fn s
bindString _ v = v

bindInteger :: (Integer -> Value) -> Value -> Value
bindInteger fn (ValueInteger i) = fn i
bindInteger _ v = v

bindDouble :: (Double -> Value) -> Value -> Value
bindDouble fn (ValueDouble d) = fn d
bindDouble _ v = v

instance Eq k => Indexed (Lookup k v) where
  type Index (Lookup k v) = k
  type Data (Lookup k v) = v
    
  hasCol name (Lookup os) = lookupHas name os
  dropCol name (Lookup os) = Lookup (lookupDrop name os)
  mapCol name fn (Lookup os) = Lookup (lookupMap name fn os)
  mapColF name fn (Lookup os) = Lookup <$> lookupMapF name fn os
  mapWithIndex f (Lookup os) = Lookup (lookupMapWithIndex f os)
  mapWithIndexF f (Lookup os) = Lookup <$> lookupMapWithIndexF f os
  mapIndex f (Lookup os) = Lookup (lookupImap f os)
  mapIndexF f (Lookup os) = Lookup <$> lookupImapF f os
  foldCol name ff (Lookup os) = F.fold ff (lookup name os)
  foldColF name ff (Lookup os) = F.foldM ff (lookup name os)
  foldWithIndex ff (Lookup os) = F.fold ff os
  foldWithIndexF ff (Lookup os) = F.foldM ff os
  foldIndex ff (Lookup os) = F.fold ff (fst <$> os)
  foldIndexF ff (Lookup os) = F.foldM ff (fst <$> os)

exampleObj :: Lookup String Value
exampleObj = Lookup
  [ ("id", ValueInteger 42)
  , ("name", ValueString "foo")
  ]

exampleRecord :: [Value]
exampleRecord =
  [ ValueInteger 42
  , ValueString "foo"
  ]

exampleHeader :: [String]
exampleHeader =
  [ "id"
  , "name"
  ]

exampleDecl :: Lookup String ValueType
exampleDecl = Lookup
  [ ("id", ValueTypeInteger)
  , ("name", ValueTypeString)
  ]
