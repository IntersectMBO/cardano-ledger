{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.ShelleyMA.Value
  ( PolicyID (..),
    AssetID (..),
    Value (..),
    insert,
    lookup,
    showValue,
  )
where

import Cardano.Binary
  ( FromCBOR,
    ToCBOR,
    encodeListLen,
    fromCBOR,
    toCBOR,
  )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.CannonicalMaps
  ( cannonicalMap,
    cannonicalMapUnion,
    pointWise,
  )
import Data.Group (Abelian, Group (..))
import Data.Map.Internal
  ( Map (..),
    link,
    link2,
    splitLookup,
  )
import Data.Map.Strict (assocs)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Prelude hiding (lookup)

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving newtype
    ( Show,
      Eq,
      ToCBOR,
      FromCBOR,
      Ord,
      NoThunks,
      NFData
    )

-- | Policy ID
newtype PolicyID era = PolicyID {policyID :: ScriptHash era}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoThunks, NFData)

-- | The Value representing MultiAssets
data Value era = Value !Integer !(Map (PolicyID era) (Map AssetID Integer))
  deriving (Show, Generic)

instance Era era => Eq (Value era) where
  x == y = pointwise (==) x y

-- TODO make these specific
instance NFData (Value era)

instance NoThunks (Value era)

instance Semigroup (Value era) where
  Value c m <> Value c1 m1 =
    Value (c + c1) (cannonicalMapUnion (cannonicalMapUnion (+)) m m1)

instance Monoid (Value era) where
  mempty = Value 0 mempty

instance Group (Value era) where
  invert (Value c m) = Value (- c) (cannonicalMap (cannonicalMap ((-1 :: Integer) *)) m)

instance Abelian (Value era)

-- ===================================================
-- Make the Val instance of Value

instance Era era => Val (Value era) where
  s <×> (Value c v) = Value (fromIntegral s * c) (cannonicalMap (cannonicalMap ((fromIntegral s) *)) v)
  isZero (Value c v) = c == 0 && Map.null v
  coin (Value c _) = Coin c
  inject (Coin c) = Value c mempty
  modifyCoin f (Value c m) = Value n m where (Coin n) = f (Coin c)
  pointwise p (Value c x) (Value d y) = (p c d) && (pointWise (pointWise p) x y)

  size (Value _ v) =
    -- add uint for the Coin portion in this size calculation
    foldr accum uint v
    where
      -- add addrHashLen for each Policy ID
      accum u ans = foldr accumIns (ans + addrHashLen) u
        where
          -- add assetIdLen and uint for each asset of that Policy ID
          accumIns _ ans1 = ans1 + assetIdLen + uint
      -- TODO move these constants somewhere (they are also specified in CDDL)
      uint :: Integer
      uint = 5

      assetIdLen :: Integer
      assetIdLen = 32

      -- address hash length is always same as Policy ID length
      addrHashLen :: Integer
      addrHashLen = 28

-- ==============================================================
-- CBOR

-- TODO filter out 0s at deserialization
-- TODO Probably the actual serialization will be of the formal Coin OR Value type
-- Maybe better to make this distinction in the TxOut de/serialization

instance
  (Era era, Typeable (Core.Script era)) =>
  ToCBOR (Value era)
  where
  toCBOR (Value c v) =
    encodeListLen 2
      <> toCBOR c
      <> toCBOR v

instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (Value era)
  where
  fromCBOR = do
    decodeRecordNamed "Value" (const 2) $ do
      c <- fromCBOR
      v <- fromCBOR
      pure $ Value c v

-- ========================================================================
-- Compactible

instance Core.Compactible (Value era) where
  -- TODO a proper compact form
  newtype CompactForm (Value era) = CompactValue {getCompactValue :: Value era}
    deriving (ToCBOR, FromCBOR)
  toCompact = CompactValue
  fromCompact = getCompactValue

-- ========================================================================
-- Operations on Values

lookup :: PolicyID era -> AssetID -> Value era -> Integer
lookup pid aid (Value _ m) =
  case Map.lookup pid m of
    Nothing -> 0
    Just m2 -> Map.findWithDefault 0 aid m2

-- | insert comb policy asset n v,
--   if comb = \ old new -> old, the integer in the Value is prefered over n
--   if comb = \ old new -> new, then n is prefered over the integer in the Value
--   if (comb old new) == 0, then that value should not be stored in the Map part of the Value.
insert :: (Integer -> Integer -> Integer) -> PolicyID era -> AssetID -> Integer -> Value era -> Value era
insert combine pid aid new (Value cn m1) =
  case splitLookup pid m1 of
    (l1, Just m2, l2) ->
      case splitLookup aid m2 of
        (v1, Just old, v2) ->
          if n == 0
            then
              let m3 = (link2 v1 v2)
               in if Map.null m3
                    then Value cn (link2 l1 l2)
                    else Value cn (link pid m3 l1 l2)
            else Value cn (link pid (link aid n v1 v2) l1 l2)
          where
            n = combine old new
        (_, Nothing, _) -> Value cn (link pid (if new == 0 then m2 else (Map.insert aid new m2)) l1 l2)
    (l1, Nothing, l2) ->
      Value
        cn
        ( if new == 0
            then link2 l1 l2
            else link pid (Map.singleton aid new) l1 l2
        )

-- ========================================================

-- | Display a Value as a String, one token per line
showValue :: Value era -> String
showValue v = show c ++ "\n" ++ unlines (map trans ts)
  where
    (c, ts) = gettriples v
    trans (PolicyID x, hash, cnt) = show x ++ ",  " ++ show hash ++ ",  " ++ show cnt

gettriples :: Value era -> (Integer, [(PolicyID era, AssetID, Integer)])
gettriples (Value c m1) = (c, foldr accum1 [] (assocs m1))
  where
    accum1 (policy, m2) ans = foldr accum2 ans (assocs m2) where accum2 (asset, cnt) ans2 = (policy, asset, cnt) : ans2
