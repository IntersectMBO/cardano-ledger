{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.ListMap
  ( ListMap (..),
    foldrWithKey,
    keys,
    keysSet,
    elems,
    lookup,
    filter,
    toMap,
    fromMap,
    mapKeys,
    map,
    empty,
    fromList,
    toList,
  )
where

import Cardano.Ledger.Binary (FromCBOR (..), ToCBOR (..), decodeMapLen, encodeMapLen)
import Control.DeepSeq (NFData, NFData1)
import Control.Monad
import Data.Aeson
  ( FromJSON (..),
    FromJSON1 (..),
    FromJSONKey (..),
    FromJSONKeyFunction (..),
    ToJSON (..),
    ToJSON1 (..),
    ToJSON2 (..),
    ToJSONKey (..),
    ToJSONKeyFunction (..),
    Value (..),
  )
import qualified Data.Aeson as J
import Data.Aeson.Encoding (dict)
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (listValue)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Generics (Generic, Generic1)
import NoThunks.Class (NoThunks)
import Prelude hiding (filter, lookup, map)
import qualified Prelude as Pre

-- | ListMap is a wrapper around an associative list. It is encoded in CBOR
--   and JSON as an object/map.
newtype ListMap k v = ListMap
  { unListMap :: [(k, v)]
  }
  deriving (Show, Foldable, Functor, Generic, Generic1, NFData)

-- | Eq works similarly to Map
instance (Ord k, Ord v) => Eq (ListMap k v) where
  (ListMap xs) == (ListMap ys) = L.sort xs == L.sort ys

instance Semigroup (ListMap k v) where
  ListMap xs <> ListMap ys = ListMap $ xs <> ys

instance Monoid (ListMap k v) where
  mempty = ListMap mempty

instance (NoThunks k, NoThunks v) => NoThunks (ListMap k v)

instance (FromCBOR k, FromCBOR v) => FromCBOR (ListMap k v) where
  fromCBOR =
    ListMap <$> do
      len <- decodeMapLen
      replicateM len $ do
        k <- fromCBOR
        v <- fromCBOR
        return (k, v)

instance (ToCBOR k, ToCBOR v) => ToCBOR (ListMap k v) where
  toCBOR (ListMap xs) = encodeMapLen (fromIntegral $ length xs) <> foldr f mempty xs
    where
      f (k, v) e = toCBOR k <> toCBOR v <> e

instance ToJSONKey k => ToJSON1 (ListMap k) where
  liftToJSON g _ = case toJSONKey of
    ToJSONKeyText f _ -> Object . KM.fromList . unListMap . mapKeyValO f g
    ToJSONKeyValue f _ -> Array . V.fromList . L.map (toJSONPair f g) . unListMap
    where
      mapKeyValO :: (k1 -> k2) -> (v1 -> v2) -> ListMap k1 v1 -> ListMap k2 v2
      mapKeyValO fk kv = ListMap . foldrWithKey (\(k, v) -> ((fk k, kv v) :)) []
      toJSONPair :: (a -> Value) -> (b -> Value) -> (a, b) -> Value
      toJSONPair a b = liftToJSON2 a (listValue a) b (listValue b)

  liftToEncoding g _ = case toJSONKey of
    ToJSONKeyText _ f -> dict f g (foldrWithKey . uncurry)
    ToJSONKeyValue _ f -> E.list (pairEncoding f) . unListMap
    where
      pairEncoding f (a, b) = E.list id [f a, g b]

instance (ToJSON v, ToJSONKey k) => ToJSON (ListMap k v) where
  toJSON = J.toJSON1

  toEncoding = J.liftToEncoding J.toEncoding J.toEncodingList

instance (FromJSONKey k) => FromJSON1 (ListMap k) where
  liftParseJSON parser _ = J.withObject "ListMap" $ \obj -> do
    let kv = KM.toList obj
    res <- forM kv $ \(k, v) -> do
      let t = Key.toText k
      k' <- case fromJSONKey of
        FromJSONKeyCoerce -> return $ coerce t
        FromJSONKeyText f -> return $ f t
        FromJSONKeyTextParser f -> f t
        -- TODO figure out what to do here
        FromJSONKeyValue _ -> error "key conversion not implemented"
      v' <- parser v
      return (k', v')
    return $ ListMap res

instance (FromJSON v, FromJSONKey k) => FromJSON (ListMap k v) where
  parseJSON = J.parseJSON1

instance NFData k => NFData1 (ListMap k)

instance Bifunctor ListMap where
  bimap f g (ListMap xs) = ListMap $ fmap (bimap f g) xs

foldrWithKey :: ((k, a) -> b -> b) -> b -> ListMap k a -> b
foldrWithKey f z = L.foldr f z . unListMap

keys :: ListMap k a -> [k]
keys (ListMap xs) = fst <$> xs

keysSet :: Ord k => ListMap k a -> Set.Set k
keysSet lm = Set.fromList $ keys lm

elems :: ListMap k a -> [a]
elems (ListMap xs) = snd <$> xs

lookup :: Eq k => k -> ListMap k v -> Maybe v
lookup k (ListMap xs) = L.lookup k xs

filter :: (k -> v -> Bool) -> ListMap k v -> ListMap k v
filter f (ListMap xs) = ListMap $ Pre.filter (uncurry f) xs

toMap :: Ord k => ListMap k v -> Map.Map k v
toMap (ListMap xs) = Map.fromList xs

fromMap :: Map.Map k v -> ListMap k v
fromMap = ListMap . Map.toList

mapKeys :: (k1 -> k2) -> ListMap k1 a -> ListMap k2 a
mapKeys f = ListMap . fmap (first f) . unListMap

map :: (a -> v) -> ListMap k a -> ListMap k v
map = fmap

empty :: ListMap k a
empty = ListMap []

fromList :: [(k, v)] -> ListMap k v
fromList = ListMap

toList :: ListMap k v -> [(k, v)]
toList = unListMap
