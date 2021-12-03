{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Compact.HashMap where

import Cardano.Crypto.Hash.Class
import Control.DeepSeq
import Data.Bifunctor (first)
import Data.Compact.KeyMap (Key, KeyMap)
import qualified Data.Compact.KeyMap as KM
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import GHC.TypeLits
import Prettyprinter (viaShow)

-- ==========================================================================

class Keyed t where
  toKey :: t -> Key
  fromKey :: Key -> t

instance Keyed Key where
  toKey = id
  fromKey = id

instance HashAlgorithm h => Keyed (Hash h a) where
  toKey h =
    case hashToPackedBytes h of
      PackedBytes8 a -> KM.Key a 0 0 0
      PackedBytes28 a b c d -> KM.Key a b c (fromIntegral d)
      PackedBytes32 a b c d -> KM.Key a b c d
      _ -> error $ "Unsupported hash size: " <> show (sizeHash (Proxy :: Proxy h))
  fromKey (KM.Key a b c d) =
    hashFromPackedBytes $
      case sameNat (Proxy :: Proxy (SizeHash h)) (Proxy :: Proxy 32) of
        Just Refl -> PackedBytes32 a b c d
        Nothing ->
          case sameNat (Proxy :: Proxy (SizeHash h)) (Proxy :: Proxy 28) of
            Just Refl -> PackedBytes28 a b c (fromIntegral d)
            Nothing ->
              case sameNat (Proxy :: Proxy (SizeHash h)) (Proxy :: Proxy 8) of
                Just Refl -> PackedBytes8 a
                Nothing -> error $ "Unsupported hash size: " <> show (sizeHash (Proxy :: Proxy h))

data HashMap k v where
  HashMap :: Keyed k => KeyMap v -> HashMap k v

instance NFData v => NFData (HashMap k v) where
  rnf (HashMap km) = rnf km

lookup :: k -> HashMap k v -> Maybe v
lookup k (HashMap m) = KM.lookup (toKey k) m

insert :: k -> v -> HashMap k v -> HashMap k v
insert k v (HashMap m) = HashMap (KM.insert (toKey k) v m)

delete :: k -> HashMap k v -> HashMap k v
delete k (HashMap m) = HashMap (KM.delete (toKey k) m)

insertWithKey :: (k -> v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWithKey combine key v (HashMap m) = HashMap (KM.insertWithKey comb (toKey key) v m)
  where
    comb k v1 v2 = combine (fromKey k) v1 v2

restrictKeys :: HashMap k v -> Set k -> HashMap k v
restrictKeys (HashMap m) set = HashMap (KM.restrictKeys m (Set.map toKey set))

withoutKeys :: HashMap k v -> Set k -> HashMap k v
withoutKeys (HashMap m) set = HashMap (KM.withoutKeys m (Set.map toKey set))

splitLookup :: k -> HashMap k a -> (HashMap k a, Maybe a, HashMap k a)
splitLookup k (HashMap m) = (HashMap a, b, HashMap c)
  where
    (a, b, c) = KM.splitLookup key m
    key = toKey k

intersection :: HashMap k v -> HashMap k v -> HashMap k v
intersection (HashMap m1) (HashMap m2) = HashMap (KM.intersection m1 m2)

intersectionWith :: (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
intersectionWith combine (HashMap m1) (HashMap m2) =
  HashMap (KM.intersectionWith combine m1 m2)

unionWithKey :: (Keyed k) => (k -> v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
unionWithKey combine (HashMap m1) (HashMap m2) = HashMap (KM.unionWithKey combine2 m1 m2)
  where
    combine2 k v1 v2 = combine (fromKey k) v1 v2

unionWith :: (Keyed k) => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
unionWith combine (HashMap m1) (HashMap m2) = HashMap (KM.unionWithKey combine2 m1 m2)
  where
    combine2 _k v1 v2 = combine v1 v2

union :: (Keyed k) => HashMap k v -> HashMap k v -> HashMap k v
union (HashMap m1) (HashMap m2) = HashMap (KM.unionWithKey combine2 m1 m2)
  where
    combine2 _k v1 _v2 = v1

foldlWithKey' :: (ans -> k -> v -> ans) -> ans -> HashMap k v -> ans
foldlWithKey' accum a (HashMap m) = KM.foldWithAscKey accum2 a m
  where
    accum2 ans k v = accum ans (fromKey k) v

size :: HashMap k v -> Int
size (HashMap m) = KM.size m

fromList :: Keyed k => [(k, v)] -> HashMap k v
fromList xs = HashMap (KM.fromList (map (first toKey) xs))
{-# INLINE fromList #-}

toList :: HashMap k v -> [(k, v)]
toList (HashMap m) = KM.foldWithDescKey (\k v ans -> (fromKey k, v) : ans) [] m

mapWithKey :: (k -> v -> u) -> HashMap k v -> HashMap k u
mapWithKey f (HashMap m) = HashMap (KM.mapWithKey (f . fromKey) m)

lookupMin :: HashMap k v -> Maybe (k, v)
lookupMin (HashMap m) = first fromKey <$> KM.lookupMin m

lookupMax :: HashMap k v -> Maybe (k, v)
lookupMax (HashMap m) = first fromKey <$> KM.lookupMax m

instance (Eq k, Eq v) => Eq (HashMap k v) where
  x == y = toList x == toList y

instance (Keyed k, Show k, Show v) => Show (HashMap k v) where
  show (HashMap m) = show (KM.ppKeyMap ((viaShow @k) . fromKey) (viaShow @v) m)
