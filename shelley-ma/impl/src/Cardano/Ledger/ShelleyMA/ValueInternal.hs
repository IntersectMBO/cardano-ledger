{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.ShelleyMA.ValueInternal
  ( PolicyID (..),
    AssetID (..),
    Value (..),
    insert,
    lookup,
    showValue,
    cannonicalInsert,
    cannonicalMapUnion,
    CanonicalZero (..),
  )
where

import Cardano.Binary
  ( FromCBOR,
    ToCBOR,
    encodeListLen,
    fromCBOR,
    toCBOR,
  )
import Cardano.Ledger.Era
import Cardano.Ledger.Val (Val (..), scale)
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData (..))
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
import Data.Map.Strict (Map, assocs)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
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

-- =====================================================================================
-- Operations on Map from keys to values that are specialised to `CanonicalZero` values.
-- A (Map k v) is (CannonicalZero v), if it never stores a zero at type v.
-- In order to do this we need to know what 'zeroC' is, and 'joinC' has to know how to
-- joining together two maps where one of its arguments might be 'zeroC'
-- This class is strictly used in the implementation, and is not observable by the user.
-- ======================================================================================

class Eq t => CanonicalZero t where
  zeroC :: t
  joinC :: t -> t -> t

instance CanonicalZero Integer where
  zeroC = 0
  joinC = (+)

instance (Eq k, Eq v, Ord k, CanonicalZero v) => CanonicalZero (Map k v) where
  zeroC = Map.empty
  joinC = cannonicalMapUnion joinC

-- Note that the class CanonicalZero and the function cannonicalMapUnion are mutually recursive.

cannonicalMapUnion ::
  (Ord k, Eq a, CanonicalZero a) =>
  (a -> a -> a) -> -- (\ left right -> ??) which side do you prefer?
  Map k a ->
  Map k a ->
  Map k a
cannonicalMapUnion _f t1 Tip = t1
cannonicalMapUnion f t1 (Bin _ k x Tip Tip) = cannonicalInsert f k x t1
cannonicalMapUnion f (Bin _ k x Tip Tip) t2 = cannonicalInsert f k x t2
cannonicalMapUnion _f Tip t2 = t2
cannonicalMapUnion f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
    Nothing ->
      if x1 == zeroC
        then link2 l1l2 r1r2
        else link k1 x1 l1l2 r1r2
    Just x2 ->
      if new == zeroC
        then link2 l1l2 r1r2
        else link k1 new l1l2 r1r2
      where
        new = (f x1 x2)
    where
      !l1l2 = cannonicalMapUnion f l1 l2
      !r1r2 = cannonicalMapUnion f r1 r2
{-# INLINEABLE cannonicalMapUnion #-}

cannonicalInsert ::
  (Ord k, Eq a, CanonicalZero a) =>
  (a -> a -> a) ->
  k ->
  a ->
  Map k a ->
  Map k a
cannonicalInsert = go
  where
    go ::
      (Ord k, Eq a, CanonicalZero a) =>
      (a -> a -> a) ->
      k ->
      a ->
      Map k a ->
      Map k a
    go _ !kx x Tip = if x == zeroC then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
      case compare kx ky of
        LT -> balanceL ky y (go f kx x l) r
        GT -> balanceR ky y l (go f kx x r)
        EQ -> if new == zeroC then link2 l r else Bin sy kx new l r
          where
            new = f y x -- Apply to value in the tree, then the new value
{-# INLINEABLE cannonicalInsert #-}

cannonicalMap :: (Ord k, Eq a, CanonicalZero a) => (a -> a) -> Map k a -> Map k a
cannonicalMap f m = Map.foldrWithKey accum Map.empty m
  where
    accum k v ans = if new == zeroC then ans else Map.insert k new ans
      where
        new = f v
{-# INLINEABLE cannonicalMap #-}

-- Pointwise comparison assuming the map is CannonicalZero, and we assume semantically that
-- the value for keys not appearing in the map is 'zeroC'

pointWise ::
  (Ord k, Eq v, CanonicalZero v) =>
  (v -> v -> Bool) ->
  Map k v ->
  Map k v ->
  Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (zeroC `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all (`p` zeroC) m
pointWise p m (Bin _ k v2 ls rs) =
  case Map.splitLookup k m of
    (lm, Just v1, rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
    _ -> False

-- CBOR

-- TODO filter out 0s at deserialization
-- TODO Probably the actual serialization will be of the formal Coin OR Value type
-- Maybe better to make this distinction in the TxOut de/serialization

instance
  (Era era) =>
  ToCBOR (Value era)
  where
  toCBOR (Value c v) =
    encodeListLen 2
      <> toCBOR c
      <> toCBOR v

instance
  (Era era) =>
  FromCBOR (Value era)
  where
  fromCBOR = do
    decodeRecordNamed "Value" (const 2) $ do
      c <- fromCBOR
      v <- fromCBOR
      pure $ Value c v

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
insert :: Era era => (Integer -> Integer -> Integer) -> PolicyID era -> AssetID -> Integer -> Value era -> Value era
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
        (v1, Nothing, v2) -> Value cn (link pid (if new == 0 then m2 else (Map.insert aid new m2)) l1 l2)
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
