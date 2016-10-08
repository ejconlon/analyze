{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

import qualified Control.Foldl as F
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

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

-- newtype Frame r c t v = Frame [(c, t)] [(r, v)]
--   deriving (Show, Eq)

class Indexed c where
  type Index c :: *
  type Data c :: *

  hasCol :: Index c -> c -> Bool
  
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

class Indexed c => Shrinkable c where
  filterCol :: (Index c -> Bool) -> c -> c
  filterColF :: Monad f => (Index c -> f Bool) -> c -> f c

-- class Indexed c => Rowed c where
--   data Row c :: *
--   foldRow :: Monad f => F.Fold (Row c) a -> c -> a
--   addRow :: Row c -> c -> c

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

instance Eq k => Shrinkable (Lookup k v) where
  filterCol p (Lookup os) = Lookup $ lookupFilter p os
  filterColF p (Lookup os) = Lookup <$> lookupFilterF p os

instance (Eq k, Ord k) => Indexed (Map k v) where
  type Index (Map k v) = k
  type Data (Map k v) = k

  hasCol name m = undefined
  mapCol name fn m = undefined
  mapColF name fn m = undefined
  mapWithIndex f m = undefined
  mapWithIndexF f m = undefined
  mapIndex f m = undefined
  mapIndexF f m = undefined
  foldCol name ff m = undefined
  foldColF name ff m = undefined
  foldWithIndex ff m = undefined
  foldWithIndexF ff m = undefined
  foldIndex ff m = undefined
  foldIndexF ff m = undefined

instance (Eq k, Ord k) => Shrinkable (Map k v) where
  filterCol p m = undefined
  filterColF p m = undefined

instance Eq k => Indexed (HashMap k v) where
  type Index (HashMap k v) = k
  type Data (HashMap k v) = k

  hasCol name m = undefined
  mapCol name fn m = undefined
  mapColF name fn m = undefined
  mapWithIndex f m = undefined
  mapWithIndexF f m = undefined
  mapIndex f m = undefined
  mapIndexF f m = undefined
  foldCol name ff m = undefined
  foldColF name ff m = undefined
  foldWithIndex ff m = undefined
  foldWithIndexF ff m = undefined
  foldIndex ff m = undefined
  foldIndexF ff m = undefined

instance Eq k => Shrinkable (HashMap k v) where
  filterCol p m = undefined
  filterColF p m = undefined

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

exampleObj2 :: Lookup String Value
exampleObj2 = Lookup
  [ ("id", ValueInteger 43)
  , ("name", ValueString "bar")
  ]

exampleColMaj :: Lookup String [(Integer, Value)]
exampleColMaj = Lookup
  [ ("id", [(80, ValueInteger 42), (81, ValueInteger 43)])
  , ("name", [(80, ValueString "foo"), (81, ValueString "bar")])
  ]

exampleRowMaj :: Lookup Integer [(String, Value)]
exampleRowMaj =
  let (Lookup o1) = exampleObj
      (Lookup o2) = exampleObj2
  in Lookup [ (80, o1), (81, o2) ]
