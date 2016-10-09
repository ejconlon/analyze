{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Analyze where

import           Control.Applicative.Free
import qualified Control.Foldl                     as F
import           Control.Monad                     ((>=>))
import           Control.Monad.Catch
import qualified Data.Aeson                        as A
import           Data.Foldable                     (toList)
import           Data.Functor.Identity             (Identity (..))
import           Data.Hashable                     (Hashable)
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.Maybe                        (fromMaybe, isJust)
import           Data.Profunctor
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Typeable
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as V
import qualified Data.Vector.Fusion.Stream.Monadic as VSM
import           Pipes                             as P

-- Preamble

type Data k = (Eq k, Hashable k, Show k, Typeable k)

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

-- Decoding

data Arg m k v a = Arg k (F.FoldM m v a) deriving (Functor)

instance Monad m => Profunctor (Arg m k) where
  dimap l r (Arg k f) = Arg k (dimap l r f)

newtype Decoder m k v a = Decoder (Ap (Arg m k v) a) deriving (Functor, Applicative)

decoderKeys :: Decoder m k v a -> [k]
decoderKeys (Decoder x) = go x
  where
    go :: Ap (Arg m k v) a -> [k]
    go (Pure _)            = []
    go (Ap (Arg k _) rest) = k : (go rest)

-- This is pretty sensitive to 'lets'
apRow :: (Data k, Monad m) => Ap (Arg m k v) a -> HashMap k v -> m a
apRow (Pure a) _ = pure a
apRow (Ap (Arg k f) rest) row = do
  let mv = HM.lookup k row
  z <- F.foldM f mv
  fz <- apRow rest row
  return (fz z)

decodeRow :: (Data k, Monad m) => Decoder m k v a -> HashMap k v -> m a
decodeRow (Decoder x) = apRow x

apCol :: (Data k, Monad m) => Ap (Arg m k v) a -> HashMap k (Vector v) -> m a
apCol (Pure a) _ = pure a
apCol (Ap (Arg k f) rest) dat = do
  let vs = fromMaybe (V.empty) (HM.lookup k dat)
  z <- F.foldM f vs
  fz <- apCol rest dat
  return (fz z)

decodeCol :: (Data k, Monad m) => Decoder m k v a -> HashMap k (Vector v) -> m a
decodeCol (Decoder x) = apCol x

-- Decoding Values

data MissingKeyError k = MissingKeyError k deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (MissingKeyError k)

data ValueTypeError k = ValueTypeError k ValueType Value deriving (Show, Eq, Typeable)
instance (Show k, Typeable k) => Exception (ValueTypeError k)

-- analogous to premapM
andThen :: Monad m => F.FoldM m v a -> (a -> m b) -> F.FoldM m v b
andThen (F.FoldM step begin done) f = F.FoldM step begin (done >=> f)

orElse :: Monad m => F.FoldM m v (Maybe a) -> m a -> F.FoldM m v a
orElse f act = f `andThen` act'
  where
    act' Nothing  = act
    act' (Just x) = pure x

require :: (Data k, MonadThrow m) => k -> (k -> v -> m a) -> Arg m k v a
require k e = Arg k (F.generalize F.head `orElse` throwM (MissingKeyError k) `andThen` e k)

textual :: (Data k, MonadThrow m) => k -> Value -> m Text
textual _ (ValueText s) = pure s
textual k v             = throwM (ValueTypeError k ValueTypeText v)

integral :: (Data k, MonadThrow m) => k -> Value -> m Integer
integral _ (ValueInteger s) = pure s
integral k v                = throwM (ValueTypeError k ValueTypeInteger v)

floating :: (Data k, MonadThrow m) => k -> Value -> m Double
floating _ (ValueDouble s) = pure s
floating k v               = throwM (ValueTypeError k ValueTypeDouble v)

-- RFrame

-- In-memory row-oriented frame
data RFrame k v = RFrame
  { rframeKeys :: !(Vector k)
  , rframeData :: !(Vector (Vector v))
  } deriving (Functor, Foldable, Traversable)

instance A.ToJSON v => A.ToJSON (RFrame Text v) where
  toJSON frame = A.Array (A.toJSON . HM.fromList . V.toList <$> rframeIter frame)

rframeCols :: RFrame k v -> Int
rframeCols (RFrame ks _) = V.length ks

rframeRows :: RFrame k v -> Int
rframeRows (RFrame _ vs) = V.length vs

rframeIter :: Data k => RFrame k v -> Vector (Vector (k, v))
rframeIter (RFrame ks vs) = V.zip ks <$> vs

rframeFold :: (Data k, Monad m) => F.FoldM m (Vector v) a -> RFrame k v -> m a
rframeFold ff (RFrame kv vs) = F.foldM ff vs

rframeDecode :: (Data k, Monad m) => Decoder m k v a -> RFrame k v -> Vector (m a)
rframeDecode decoder rframe = decodeRow decoder . HM.fromList . V.toList <$> rframeIter rframe

rframeFilter :: (Vector (k, v) -> Bool) -> RFrame k v -> RFrame k v
rframeFilter = undefined

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

-- TODO Just write all fields as is
instance A.ToJSON v => A.ToJSON (CFrame Text v) where
  toJSON frame = undefined

cframeCols :: CFrame k v -> Int
cframeCols (CFrame ks _ _) = V.length ks

cframeDecode :: (Data k, Monad m) => Decoder m k v a -> CFrame k v -> m a
cframeDecode decoder (CFrame _ _ dat) = decodeCol decoder dat

-- Merge two CFrames with the given col merge function if overlapping
-- Retains the order of columns as seen in the first then second (minus repeats)
-- Will throw on row length mismatch
cframeMerge :: MonadThrow m => (k -> v -> v -> v) -> CFrame k v -> CFrame k v -> m (CFrame k v)
cframeMerge = undefined

cframeFilter :: (k -> Bool) -> CFrame k v -> CFrame k v
cframeFilter p (CFrame ks rs cs) = CFrame ks' rs cs'
  where
    ks' = V.filter p ks
    p' k _ = p k
    cs' = HM.filterWithKey p' cs

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

pframeFold :: Monad m => F.FoldM m (Vector v) a -> PFrame m k v -> m a
pframeFold = undefined

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
          Left ()       -> Nothing
          Right (a, p') -> Just (a, p')
    result = (\vl -> RFrame ks (V.fromList vl)) <$> VSM.toList unfolded

project :: (Data k, MonadThrow m) => Vector k -> HashMap k v -> m (Vector v)
project ks row = V.mapM f ks
  where
    f k =
      case HM.lookup k row of
        Nothing -> throwM (MissingKeyError k)
        Just v  -> pure v

-- kind of the inverse of rframeIter
projectRow :: (Data k, MonadThrow m) => Vector k -> Vector (HashMap k v) -> m (RFrame k v)
projectRow ks rs = (\vs -> RFrame ks vs) <$> V.mapM (project ks) rs

rowToCol :: Data k => RFrame k v -> CFrame k v
rowToCol (RFrame ks vs) = CFrame ks (V.length vs) dat
  where
    dat = HM.fromList (V.toList (select <$> V.indexed ks))
    select (i, k) = (k, (V.!i) <$> vs)

colToRow :: Data k => CFrame k v -> RFrame k v
colToRow (CFrame ks rs dat) = RFrame ks vs
  where
    vs = V.generate rs f
    f i = (\k -> (dat HM.! k) V.! i) <$> ks

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

-- maxId :: MonadThrow m => F.FoldM m Text Integer
-- maxId = require "id" integral

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
