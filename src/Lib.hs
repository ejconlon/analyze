{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

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

newtype Object = Object [(String, Value)]
  deriving (Show, Eq)
  
newtype Record = Record [Value]
  deriving (Show, Eq)
  
newtype Header = Header [String]
  deriving (Show, Eq)
  
newtype Decl = Decl [(String, ValueType)]
  deriving (Show, Eq)
  
data Frame = Frame Decl [Record]
  deriving (Show, Eq)

class Matrixy c where
  type Index c :: *
  type Data c :: *
  type Row c :: *
  
  columns :: c -> [Index c]
  mapIndex :: (Index c -> Index c) -> c -> c
  mapIndexF :: Applicative f => (Index c -> f (Index c)) -> c -> f c
  mapCol :: Index c -> (Data c -> Data c) -> c -> c
  mapColF :: Applicative f => Index c -> (Data c -> f (Data c)) -> c -> f c
  hasCol :: Index c -> c -> Bool
  dropCol :: Index c -> c -> c
  setCol :: Index c -> Row c -> c -> c  
  --foldCol :: Index c -> Fold (Data c) m -> c -> m
  --foldAll :: Fold (Row c) m -> c -> m

testMatrixy :: c -> Bool
testMatrixy = undefined

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

objToRec :: Object -> Record
objToRec (Object os) = Record (snd <$> os)

objToHead :: Object -> Header
objToHead (Object os) = Header (fst <$> os)

objToDecl :: Object -> Decl
objToDecl (Object os) = Decl ((valueToType <$>) <$> os)

instance Matrixy Object where
  type Index Object = String
  type Data Object = Value
  type Row Object = Value
    
  columns (Object os) = fst <$> os
  mapIndex f (Object os) = Object (lookupImap f os)
  mapIndexF f (Object os) = Object <$> lookupImapF f os
  mapCol name fn (Object os) = Object (lookupMap name fn os)
  mapColF name fn (Object os) = Object <$> lookupMapF name fn os
  hasCol name (Object os) = lookupHas name os
  dropCol name (Object os) = Object (lookupDrop name os)
  setCol name value = mapCol name (const value)

--instance Matrixy Frame where
--  type Index Frame = String
--  type Data Frame = Value
--  type Row Frame = Record
--  
--  columns = undefined
--  mapCol = undefined
--  mapColF = undefined
--  hasCol = undefined
--  dropCol = undefined
--  setCol = undefined

mapRow :: (Record -> Record) -> (Frame -> Frame)
mapRow = undefined

-- Fixtures

exampleObj :: Object
exampleObj = Object
  [ ("id", ValueInteger 42)
  , ("name", ValueString "foo")
  ]

exampleRecord :: Record
exampleRecord = Record
  [ ValueInteger 42
  , ValueString "foo"
  ]

exampleHeader :: Header
exampleHeader = Header
  [ "id"
  , "name"
  ]

exampleDecl :: Decl
exampleDecl = Decl
  [ ("id", ValueTypeInteger)
  , ("name", ValueTypeString)
  ]
  
-- Tests

test :: Bool
test = 
  (objToRec exampleObj == exampleRecord) &&
  (objToHead exampleObj == exampleHeader) &&
  (objToDecl exampleObj == exampleDecl)
