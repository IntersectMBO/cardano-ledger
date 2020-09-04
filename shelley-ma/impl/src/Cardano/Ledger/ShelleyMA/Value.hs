{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.ShelleyMA.Value
  ( Quantity (..),
    PolicyID (..),
    AssetID (..),
    Value (..),
  )
where

import Cardano.Binary
  ( FromCBOR,
    ToCBOR,
    encodeListLen,
    fromCBOR,
    toCBOR,
  )
import Cardano.Prelude (NFData (), NoUnexpectedThunks (..))
import Data.ByteString (ByteString)
import Data.Group (Abelian, Group (..))
import Data.Map.Internal
  ( Map (..),
    balanceL,
    balanceR,
    link,
    link2,
    singleton,
    splitLookup,
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.PartialOrd (PartialOrd)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Cardano.Ledger.Crypto
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Val (Val)
import qualified Cardano.Ledger.Val as Val

-- ============================================================================
-- Multi Assests
--
-- A Value is a map from 'PolicyID's to a quantity of assets with this policy.
-- This map implements a finitely supported functions ovr PolicyId. A PolicyID
-- is not stored in the Map, then its quantity is assumed to be 0.
--
-- Operations on assets are usually implemented 'pointwise'. That is, we apply
-- the operation to the quantities for each asset in turn. So when we add two
-- 'Value's the resulting 'Value' has, for each asset, the sum of the quantities
-- of /that particular/ asset in the argument 'Value'. The effect of this is
-- that the assets in the 'Value' are "independent", and are operated on
-- separately.
--
-- We can think of 'Value' as a vector space whose dimensions are assets. At the
-- moment there is only a single asset type (Ada), so 'Value' contains
-- one-dimensional vectors. When asset-creating transactions are implemented,
-- this will change and the definition of 'Value' will change to a 'Map Asset
-- Int', effectively a vector with infinitely many dimensions whose non-zero
-- values are recorded in the map.
--
-- To create a value of 'Value', we need to specifiy an asset policy. This can
-- be done using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we
-- use 'Ledger.Ada.fromValue'. Plutus contract authors will be able to define
-- modules similar to 'Ledger.Ada' for their own assets.
-- ============================================================================

-- | Quantity
newtype Quantity = Quantity {unInt :: Integer}
  deriving newtype
    ( Show,
      Enum,
      Eq,
      FromCBOR,
      NFData,
      NoUnexpectedThunks,
      Ord,
      PartialOrd,
      ToCBOR
    )
  deriving stock
    (Generic)
  deriving (Semigroup, Monoid, Group, Abelian) via Sum Integer
  deriving (Val) via Coin

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving newtype
    ( Show,
      Eq,
      ToCBOR,
      FromCBOR,
      Ord,
      NoUnexpectedThunks,
      NFData
    )

-- | Policy ID
newtype PolicyID era = PolicyID {policyID :: ScriptHash era}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

-- | The Value representing MultiAssets
data Value era = Value !Coin !(Map (PolicyID era) (Map AssetID Quantity))
  deriving (Eq, Show, Generic)

instance NFData (Value era)

instance NoUnexpectedThunks (Value era)

instance Semigroup (Value era) where
  Value c m <> Value c1 m1 =
    Value (c <> c1) (unionWithV (unionWithV (<>)) m m1)

instance Monoid (Value era) where
  mempty = Value mempty mempty

instance Group (Value era) where
  invert (Value c m) = Value (invert c) (mapV (mapV invert) m)

instance Abelian (Value era)

instance PartialOrd (Value era) where
  (Value c m) <= (Value c1 m1) =
    c Val.<= c1 && pointWise (pointWise (Val.<=)) m m1

instance Typeable era => Val (Value era) where
  scale s (Value c v) = Value (Val.scale s c) (mapV (mapV $ Val.scale s) v)
  coin (Value c _) = c
  inject c = Value c mempty
  size (Value _ v) =
    -- add uint for the Coin portion in this size calculation
    foldr accum uint v
    where
      -- add addrHashLen for each Policy ID
      accum u ans = foldr accumIns (ans + addrHashLen) u
        where
          -- add assetIdLen and uint for each asset of that Policy ID
          accumIns _ ans1 = ans1 + assetIdLen + uint

-- ============================================================================
-- Operations on Map, specialised to comparable `Monoid` values.
-- ============================================================================

-- Pointwise comparison assuming the map is the Default value everywhere except
-- where it is defined
pointWise ::
  (Ord k, Eq v, Monoid v) =>
  (v -> v -> Bool) ->
  Map k v ->
  Map k v ->
  Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (mempty `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all (`p` mempty) m
pointWise p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
    _ -> False

-- The following functions enforce the invariant that mempty is never stored in a
-- Map
insertWithV ::
  (Ord k, Eq a, Monoid a) =>
  (a -> a -> a) ->
  k ->
  a ->
  Map k a ->
  Map k a
insertWithV = go
  where
    go ::
      (Ord k, Eq a, Monoid a) =>
      (a -> a -> a) ->
      k ->
      a ->
      Map k a ->
      Map k a
    go _ !kx x Tip = if x == mempty then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
      case compare kx ky of
        LT -> balanceL ky y (go f kx x l) r
        GT -> balanceR ky y l (go f kx x r)
        EQ -> if new == mempty then link2 l r else Bin sy kx new l r
          where
            new = f x y
{-# INLINEABLE insertWithV #-}

unionWithV ::
  (Ord k, Eq a, Monoid a) =>
  (a -> a -> a) ->
  Map k a ->
  Map k a ->
  Map k a
unionWithV _f t1 Tip = t1
unionWithV f t1 (Bin _ k x Tip Tip) = insertWithV f k x t1
unionWithV f (Bin _ k x Tip Tip) t2 = insertWithV f k x t2
unionWithV _f Tip t2 = t2
unionWithV f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
    Nothing ->
      if x1 == mempty
        then link2 l1l2 r1r2
        else link k1 x1 l1l2 r1r2
    Just x2 ->
      if new == mempty
        then link2 l1l2 r1r2
        else link k1 new l1l2 r1r2
      where
        new = (f x1 x2)
    where
      !l1l2 = unionWithV f l1 l2
      !r1r2 = unionWithV f r1 r2
{-# INLINEABLE unionWithV #-}

mapV :: (Ord k, Eq a, Monoid a) => (a -> a) -> Map k a -> Map k a
mapV f m = Map.foldrWithKey accum Map.empty m
  where
    accum k v ans = if new == mempty then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE mapV #-}

-- CBOR

-- TODO filter out 0s at deserialization
-- TODO Probably the actual serialization will be of the formal Coin OR Value type
-- Maybe better to make this distinction in the TxOut de/serialization

instance
  (Crypto crypto)
  => ToCBOR (Value crypto)
 where
   toCBOR (Value c v) =
           encodeListLen 2
           <> toCBOR c
           <> toCBOR v


instance
  (Crypto crypto)
  => FromCBOR (Value crypto)
 where
  fromCBOR = do
    decodeRecordNamed "Value" (const 2) $ do
      c <- fromCBOR
      v <- fromCBOR
      pure $ Value c v
