{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Mary.Value
  ( PolicyID (..),
    AssetName (..),
    Value (..),
    insert,
    lookup,
    policies,
    prune,
    showValue,
    valueFromList,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR,
    ToCBOR,
    TokenType (..),
    decodeInteger,
    decodeWord64,
    fromCBOR,
    peekTokenType,
    toCBOR,
  )
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Compactible (Compactible (..))
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Pretty (PDoc, PrettyA (..), ppCoin, ppInteger, ppList, ppLong, ppScriptHash, ppSexp)
import Cardano.Ledger.Torsor (Torsor (..))
import Cardano.Ledger.Val
  ( DecodeMint (..),
    DecodeNonNegative (..),
    EncodeMint (..),
    Val (..),
  )
import Cardano.Prelude (cborError)
import Control.DeepSeq (NFData (..))
import Control.Monad (guard)
import Data.Array (Array)
import Data.Array.IArray (array)
import qualified Data.ByteString as BS
import Data.CanonicalMaps
  ( canonicalMap,
    canonicalMapUnion,
    pointWise,
  )
import Data.Coders
  ( Decode (..),
    Encode (..),
    decode,
    encode,
    (!>),
    (<!),
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
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..))
import Prettyprinter (hsep)
import Shelley.Spec.Ledger.Coin (Coin (..), integerToWord64)
import Shelley.Spec.Ledger.Scripts (ScriptHash (..))
import Shelley.Spec.Ledger.Serialization (decodeMap, encodeMap)
import Prelude hiding (lookup)

-- | Asset Name
newtype AssetName = AssetName {assetName :: BS.ByteString}
  deriving newtype
    ( Show,
      Eq,
      ToCBOR,
      Ord,
      NoThunks,
      NFData
    )

instance FromCBOR AssetName where
  fromCBOR = do
    an <- fromCBOR
    if BS.length an > 32
      then cborError $ DecoderErrorCustom "asset name exceeds 32 bytes:" (decodeUtf8 an)
      else pure . AssetName $ an

-- | Policy ID
newtype PolicyID crypto = PolicyID {policyID :: ScriptHash crypto}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoThunks, NFData)

-- | The Value representing MultiAssets
data Value crypto = Value !Integer !(Map (PolicyID crypto) (Map AssetName Integer))
  deriving (Show, Generic)

instance CC.Crypto crypto => Eq (Value crypto) where
  x == y = pointwise (==) x y

-- TODO make these specific
instance NFData (Value crypto)

instance NoThunks (Value crypto)

instance Semigroup (Value crypto) where
  Value c m <> Value c1 m1 =
    Value (c + c1) (canonicalMapUnion (canonicalMapUnion (+)) m m1)

instance Monoid (Value crypto) where
  mempty = Value 0 mempty

instance Group (Value crypto) where
  invert (Value c m) =
    Value
      (- c)
      (canonicalMap (canonicalMap ((-1 :: Integer) *)) m)

instance Abelian (Value crypto)

-- ===================================================
-- Make the Val instance of Value

instance CC.Crypto crypto => Val (Value crypto) where
  s <×> (Value c v) =
    Value
      (fromIntegral s * c)
      (canonicalMap (canonicalMap ((fromIntegral s) *)) v)
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
      accum u ans = foldr accumIns (ans + policyIdLen) u
        where
          -- add assetNameLen and uint for each asset of that Policy ID
          accumIns _ ans1 = ans1 + assetNameLen + uint
      -- TODO move these constants somewhere (they are also specified in CDDL)
      uint :: Integer
      uint = 9

      assetNameLen :: Integer
      assetNameLen = 32

      -- TODO dig up these constraints from Era
      -- address hash length is always same as Policy ID length
      policyIdLen :: Integer
      policyIdLen = 28

-- ==============================================================
-- CBOR

-- TODO filter out 0s at deserialization
-- TODO Probably the actual serialization will be of the formal Coin OR Value type
-- Maybe better to make this distinction in the TxOut de/serialization

decodeValue ::
  CC.Crypto crypto =>
  Decoder s (Value crypto)
decodeValue = do
  tt <- peekTokenType
  case tt of
    TypeUInt -> inject . Coin <$> decodeInteger
    TypeUInt64 -> inject . Coin <$> decodeInteger
    TypeNInt -> inject . Coin <$> decodeInteger
    TypeNInt64 -> inject . Coin <$> decodeInteger
    TypeListLen -> decodeValuePair decodeInteger
    TypeListLen64 -> decodeValuePair decodeInteger
    TypeListLenIndef -> decodeValuePair decodeInteger
    _ -> fail $ "Value: expected array or int, got " ++ show tt

decodeValuePair ::
  CC.Crypto crypto =>
  (forall t. Decoder t Integer) ->
  Decoder s (Value crypto)
decodeValuePair decodeAmount =
  decode $
    RecD Value
      <! D decodeAmount
      <! D (decodeMultiAssetMaps decodeAmount)

encodeMultiAssetMaps ::
  CC.Crypto crypto =>
  Map (PolicyID crypto) (Map AssetName Integer) ->
  Encoding
encodeMultiAssetMaps = encodeMap toCBOR (encodeMap toCBOR toCBOR)

decodeMultiAssetMaps ::
  CC.Crypto crypto =>
  Decoder s Integer ->
  Decoder s (Map (PolicyID crypto) (Map AssetName Integer))
decodeMultiAssetMaps decodeAmount =
  prune <$> decodeMap fromCBOR (decodeMap fromCBOR decodeAmount)

decodeNonNegativeInteger :: Decoder s Integer
decodeNonNegativeInteger = fromIntegral <$> decodeWord64

decodeNonNegativeValue ::
  CC.Crypto crypto =>
  Decoder s (Value crypto)
decodeNonNegativeValue = do
  tt <- peekTokenType
  case tt of
    TypeUInt -> inject . Coin <$> decodeNonNegativeInteger
    TypeUInt64 -> inject . Coin <$> decodeNonNegativeInteger
    TypeListLen -> decodeValuePair decodeNonNegativeInteger
    TypeListLen64 -> decodeValuePair decodeNonNegativeInteger
    TypeListLenIndef -> decodeValuePair decodeNonNegativeInteger
    _ -> fail $ "Value: expected array or int, got " ++ show tt

instance
  CC.Crypto crypto =>
  ToCBOR (Value crypto)
  where
  toCBOR (Value c v) =
    if Map.null v
      then toCBOR c
      else
        encode $
          Rec Value
            !> To c
            !> E encodeMultiAssetMaps v

instance
  CC.Crypto crypto =>
  FromCBOR (Value crypto)
  where
  fromCBOR = decodeValue

instance
  CC.Crypto crypto =>
  DecodeNonNegative (Value crypto)
  where
  decodeNonNegative = decodeNonNegativeValue

instance
  CC.Crypto crypto =>
  DecodeMint (Value crypto)
  where
  decodeMint = Value 0 <$> decodeMultiAssetMaps decodeInteger

instance
  CC.Crypto crypto =>
  EncodeMint (Value crypto)
  where
  encodeMint (Value _ multiasset) = encodeMultiAssetMaps multiasset

-- ========================================================================
-- Compactible
-- This is used in the TxOut which stores the (CompactForm Value).

instance CC.Crypto crypto => Compactible (Value crypto) where
  newtype CompactForm (Value crypto) = CompactValue (CV crypto)
    deriving (Eq, Typeable, Show, NoThunks, ToCBOR, FromCBOR)
  toCompact x = CompactValue <$> toCV x
  fromCompact (CompactValue x) = fromCV x

instance CC.Crypto crypto => ToCBOR (CV crypto) where
  toCBOR = toCBOR . fromCV

instance CC.Crypto crypto => FromCBOR (CV crypto) where
  fromCBOR = do
    v <- decodeNonNegativeValue
    case toCV v of
      Nothing ->
        fail
          "impossible failure: decoded nonnegative value that cannot be compacted"
      Just x -> pure x

data CV crypto
  = CV
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !(Array Int (CVPart crypto))
  deriving (Eq, Show, Typeable)

deriving via OnlyCheckWhnfNamed "CV" (CV crypto) instance NoThunks (CV crypto)

data CVPart crypto
  = CVPart
      !(PolicyID crypto)
      {-# UNPACK #-} !AssetName
      {-# UNPACK #-} !Word64
  deriving (Eq, Show, Typeable)

deriving via
  OnlyCheckWhnfNamed "CVPart" (CVPart crypto)
  instance
    NoThunks (CVPart crypto)

toCV :: Value crypto -> Maybe (CV crypto)
toCV v = do
  let (c, triples) = gettriples v
      policyIDs = Set.fromList $ (\(x, _, _) -> x) <$> triples
      n = length triples - 1
  cvParts <- traverse (toCVPart policyIDs) triples
  let arr = array (0, n) (zip [0 .. n] cvParts)
  c' <- integerToWord64 c
  pure $ CV c' arr
  where
    deduplicate xs x = fromMaybe x $ do
      r <- Set.lookupLE x xs
      guard (x == r)
      pure r
    toCVPart policyIdSet (policyId, aname, amount) =
      CVPart (deduplicate policyIdSet policyId) aname
        <$> integerToWord64 amount

fromCV :: CC.Crypto crypto => CV crypto -> Value crypto
fromCV (CV w vs) = foldr f (inject . Coin . fromIntegral $ w) vs
  where
    f (CVPart policyId aname amount) acc =
      insert (+) policyId aname (fromIntegral amount) acc

instance CC.Crypto crypto => Torsor (Value crypto) where
  -- TODO a proper torsor form
  type Delta (Value crypto) = (Value crypto)
  addDelta = (<+>)
  toDelta = id

-- ========================================================================
-- Operations on Values

-- | Extract the set of policies in the Value.
--
--   This function is equivalent to computing the support of the value in the
--   spec.
policies :: Value crypto -> Set (PolicyID crypto)
policies (Value _ m) = Map.keysSet m

lookup :: PolicyID crypto -> AssetName -> Value crypto -> Integer
lookup pid aid (Value _ m) =
  case Map.lookup pid m of
    Nothing -> 0
    Just m2 -> Map.findWithDefault 0 aid m2

-- | insert comb policy asset n v,
--   if comb = \ old new -> old, the integer in the Value is prefered over n
--   if comb = \ old new -> new, then n is prefered over the integer in the Value
--   if (comb old new) == 0, then that value should not be stored in the Map part of the Value.
insert ::
  (Integer -> Integer -> Integer) ->
  PolicyID crypto ->
  AssetName ->
  Integer ->
  Value crypto ->
  Value crypto
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
        (_, Nothing, _) ->
          Value
            cn
            ( link
                pid
                ( if new == 0
                    then m2
                    else (Map.insert aid new m2)
                )
                l1
                l2
            )
    (l1, Nothing, l2) ->
      Value
        cn
        ( if new == 0
            then link2 l1 l2
            else link pid (Map.singleton aid new) l1 l2
        )

-- ========================================================

-- | Remove 0 assets from a map
prune ::
  Map (PolicyID crypto) (Map AssetName Integer) ->
  Map (PolicyID crypto) (Map AssetName Integer)
prune assets =
  Map.filter (not . null) $ Map.filter (/= 0) <$> assets

-- | Rather than using prune to remove 0 assets, when can avoid adding them in the
--   first place by using valueFromList to construct a Value.
valueFromList :: Integer -> [(PolicyID era, AssetName, Integer)] -> Value era
valueFromList ada =
  foldr
    (\(p, n, i) ans -> insert (+) p n i ans)
    (Value ada Map.empty)

-- | Display a Value as a String, one token per line
showValue :: Value crypto -> String
showValue v = show c ++ "\n" ++ unlines (map trans ts)
  where
    (c, ts) = gettriples v
    trans (PolicyID x, hash, cnt) =
      show x
        ++ ",  "
        ++ show hash
        ++ ",  "
        ++ show cnt

gettriples :: Value crypto -> (Integer, [(PolicyID crypto, AssetName, Integer)])
gettriples (Value c m1) = (c, triples)
  where
    triples =
      [ (policyId, aname, amount)
        | (policyId, m2) <- assocs m1,
          (aname, amount) <- assocs m2
      ]

-- =====================================
-- Pretty printing functions

ppValue :: Value crypto -> PDoc
ppValue v = ppSexp "Value" [ppCoin (Coin n), ppList pptriple triples]
  where
    (n, triples) = gettriples v
    pptriple (i, asset, num) = hsep [ppPolicyID i, ppAssetName asset, ppInteger num]

ppPolicyID :: PolicyID crypto -> PDoc
ppPolicyID (PolicyID sh) = ppScriptHash sh

ppAssetName :: AssetName -> PDoc
ppAssetName (AssetName bs) = ppLong bs

instance PrettyA (Value crypto) where prettyA = ppValue

instance PrettyA (PolicyID crypto) where prettyA x = ppSexp "PolicyID" [ppPolicyID x]

instance PrettyA AssetName where prettyA x = ppSexp "AssetName" [ppAssetName x]
