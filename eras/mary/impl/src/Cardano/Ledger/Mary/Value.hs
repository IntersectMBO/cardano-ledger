{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Mary.Value (
  PolicyID (..),
  AssetName (..),
  MaryValue (..),
  MultiAsset (..),
  insert,
  insertMultiAsset,
  lookup,
  lookupMultiAsset,
  multiAssetFromList,
  policies,
  prune,
  representationSize,
  showValue,
  flattenMultiAsset,
  valueFromList,
  ToExpr (..),
  CompactValue (..),
  isMultiAssetSmallEnough,
)
where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  DecoderError (..),
  EncCBOR (..),
  Encoding,
  TokenType (..),
  cborError,
  decodeInteger,
  decodeMap,
  decodeWord64,
  ifDecoderVersionAtLeast,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), decodePositiveCoin, integerToWord64, word64ToCoin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Crypto (Crypto (ADDRHASH))
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (..))
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..), deepseq, rwhnf)
import Control.Exception (assert)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (ToJSONKey (..), toJSONKeyText)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.CanonicalMaps (
  canonicalMap,
  canonicalMapUnion,
  pointWise,
 )
import Data.Foldable (foldMap')
import Data.Group (Abelian, Group (..))
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map.Internal (
  link,
  link2,
 )
import Data.Map.Strict (assocs)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Monoid as M (Sum (Sum, getSum))
import qualified Data.Primitive.ByteArray as BA
import Data.Proxy (Proxy (..))
import qualified Data.Semigroup as Semigroup (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..))
import Prelude hiding (lookup)

-- | Asset Name
newtype AssetName = AssetName {assetName :: SBS.ShortByteString}
  deriving newtype
    ( Eq
    , EncCBOR
    , Ord
    , NoThunks
    , NFData
    )

instance Show AssetName where
  show = show . assetNameToBytesAsHex

assetNameToBytesAsHex :: AssetName -> BS.ByteString
assetNameToBytesAsHex = BS16.encode . SBS.fromShort . assetName

assetNameToTextAsHex :: AssetName -> Text
assetNameToTextAsHex = decodeLatin1 . assetNameToBytesAsHex

instance DecCBOR AssetName where
  decCBOR = do
    an <- decCBOR
    if SBS.length an > 32
      then
        cborError $
          DecoderErrorCustom "asset name exceeds 32 bytes:" $
            assetNameToTextAsHex $
              AssetName an
      else pure $ AssetName an

-- | Policy ID
newtype PolicyID c = PolicyID {policyID :: ScriptHash c}
  deriving
    ( Show
    , Eq
    , Ord
    , Generic
    , NoThunks
    , NFData
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , ToJSONKey
    , FromJSONKey
    )

-- | The MultiAssets map
newtype MultiAsset c = MultiAsset (Map (PolicyID c) (Map AssetName Integer))
  deriving (Show, Generic, ToJSON)

instance Crypto c => Eq (MultiAsset c) where
  MultiAsset x == MultiAsset y = pointWise (pointWise (==)) x y

instance NFData (MultiAsset cypto) where
  rnf (MultiAsset m) = rnf m

instance NoThunks (MultiAsset c)

instance Semigroup (MultiAsset c) where
  MultiAsset m1 <> MultiAsset m2 =
    MultiAsset (canonicalMapUnion (canonicalMapUnion (+)) m1 m2)

instance Monoid (MultiAsset c) where
  mempty = MultiAsset mempty

instance Group (MultiAsset c) where
  invert (MultiAsset m) =
    MultiAsset (canonicalMap (canonicalMap ((-1 :: Integer) *)) m)

instance Crypto c => DecCBOR (MultiAsset c) where
  decCBOR = decodeMultiAssetMint

instance Crypto c => EncCBOR (MultiAsset c) where
  encCBOR = encodeMultiAssetMaps

-- | The Value representing MultiAssets
data MaryValue c = MaryValue !Integer !(MultiAsset c)
  deriving (Show, Generic)

instance Crypto c => Eq (MaryValue c) where
  x == y = pointwise (==) x y

instance NFData (MaryValue c) where
  rnf (MaryValue c m) = c `deepseq` rnf m

instance NoThunks (MaryValue c)

instance Semigroup (MaryValue c) where
  MaryValue c1 m1 <> MaryValue c2 m2 =
    MaryValue (c1 + c2) (m1 <> m2)

instance Monoid (MaryValue c) where
  mempty = MaryValue 0 mempty

instance Group (MaryValue c) where
  invert (MaryValue c m) =
    MaryValue
      (-c)
      (invert m)

instance Abelian (MaryValue c)

-- ===================================================
-- Make the Val instance of MaryValue

instance Crypto c => Val (MaryValue c) where
  s <×> MaryValue c (MultiAsset m) =
    MaryValue
      (fromIntegral s * c)
      (MultiAsset (canonicalMap (canonicalMap (fromIntegral s *)) m))
  isZero (MaryValue c (MultiAsset m)) = c == 0 && Map.null m
  coin (MaryValue c _) = Coin c
  inject (Coin c) = MaryValue c (MultiAsset Map.empty)
  modifyCoin f (MaryValue c m) = MaryValue n m where (Coin n) = f (Coin c)
  pointwise p (MaryValue c (MultiAsset x)) (MaryValue d (MultiAsset y)) = p c d && pointWise (pointWise p) x y

  -- returns the size, in Word64's, of the CompactValue representation of MaryValue
  size vv@(MaryValue _ (MultiAsset m))
    -- when MaryValue contains only ada
    -- !WARNING! This branch is INCORRECT in the Mary era and should ONLY be
    -- used in the Alonzo ERA.
    -- TODO - find a better way to reconcile the mistakes in Mary with what needs
    -- to be the case in Alonzo.
    | Map.null m = 2
    -- when MaryValue contains ada as well as other tokens
    -- sums up :
    -- i) adaWords : the space taken up by the ada amount
    -- ii) numberMulAssets : the space taken by number of words used to store
    --    number of non-ada assets in a value
    -- iii) the space taken up by the rest of the representation (quantities,
    --    PIDs, AssetNames, indeces)
    | otherwise =
        fromIntegral
          ( roundupBytesToWords (representationSize (snd $ gettriples vv))
              + repOverhead
          )

  isAdaOnly (MaryValue _ (MultiAsset m)) = Map.null m

  isAdaOnlyCompact = \case
    CompactValue (CompactValueAdaOnly _) -> True
    CompactValue CompactValueMultiAsset {} -> False

  coinCompact = \case
    CompactValue (CompactValueAdaOnly cc) -> cc
    CompactValue (CompactValueMultiAsset cc _ _) -> cc

  injectCompact = CompactValue . CompactValueAdaOnly

  modifyCompactCoin f = \case
    CompactValue (CompactValueAdaOnly cc) ->
      CompactValue (CompactValueAdaOnly (f cc))
    CompactValue (CompactValueMultiAsset cc n sbs) ->
      CompactValue (CompactValueMultiAsset (f cc) n sbs)

-- space (in Word64s) taken up by the ada amount
adaWords :: Int
adaWords = 1

-- 64 bit machine Word64 length
wordLength :: Int
wordLength = 8

-- overhead in MA compact rep
repOverhead :: Int
repOverhead = 4 + adaWords + numberMulAssets

-- number of words used to store number of MAs in a value
numberMulAssets :: Int
numberMulAssets = 1

-- converts bytes to words (rounding up)
roundupBytesToWords :: Int -> Int
roundupBytesToWords b = quot (b + wordLength - 1) wordLength

-- ==============================================================
-- CBOR

-- TODO filter out 0s at deserialization
-- TODO Probably the actual serialization will be of the formal Coin OR Value type
-- Maybe better to make this distinction in the TxOut de/serialization

decodeValue ::
  forall c s.
  Crypto c =>
  Decoder s (MaryValue c)
decodeValue =
  ifDecoderVersionAtLeast
    (natVersion @9)
    (decode' conwayCoin)
    (decode' preConwayCoin)
  where
    conwayCoin :: forall t. String -> Decoder t Coin
    conwayCoin = decodePositiveCoin
    preConwayCoin :: forall t. String -> Decoder t Coin
    preConwayCoin _ = word64ToCoin <$> decodeWord64
    decode' :: (forall t. String -> Decoder t Coin) -> Decoder s (MaryValue c)
    decode' decodeCoin = do
      tt <- peekTokenType
      case tt of
        TypeUInt -> inject <$> decodeCoin "Coin in Value"
        TypeUInt64 -> inject <$> decodeCoin "Coin in Value"
        TypeListLen -> decodeValuePair (fmap unCoin . decodeCoin)
        TypeListLen64 -> decodeValuePair (fmap unCoin . decodeCoin)
        TypeListLenIndef -> decodeValuePair (fmap unCoin . decodeCoin)
        _ -> fail $ "Value: expected array or int, got " ++ show tt

decodeValuePair ::
  Crypto c =>
  (forall t. String -> Decoder t Integer) ->
  Decoder s (MaryValue c)
decodeValuePair decodeAmount =
  decode $
    RecD MaryValue
      <! D (decodeAmount "Coin in Value")
      <! D (decodeMultiAssetMaps (decodeAmount "MultiAsset in Value"))

encodeMultiAssetMaps ::
  Crypto c =>
  MultiAsset c ->
  Encoding
encodeMultiAssetMaps (MultiAsset m) = encCBOR m

-- | `MultiAsset` can be used in two different circumstances:
-- during minting and in `MaryValue` while sending.
-- In the latter case, MultiAsset amounts cannot be negative,
-- while in minting negative indicates burning, and should not be zero.
decodeMultiAssetMaps :: Crypto c => (forall t. Decoder t Integer) -> Decoder s (MultiAsset c)
decodeMultiAssetMaps decodeAmount = do
  ma <- decodeMap decCBOR (decodeMap decCBOR decodeAmount)
  if isMultiAssetSmallEnough (MultiAsset ma)
    then
      ifDecoderVersionAtLeast
        (natVersion @9)
        (MultiAsset ma <$ forM_ ma (\m -> when (Map.null m) $ fail "Empty Assets are not allowed"))
        (pure $ MultiAsset $ prune ma)
    else fail "MultiAsset too big to compact"

decodeMultiAssetMint :: Crypto c => Decoder s (MultiAsset c)
decodeMultiAssetMint = decodeMultiAssetMaps decodeIntegerBounded64

instance Crypto c => EncCBOR (MaryValue c) where
  encCBOR (MaryValue c ma@(MultiAsset m)) =
    if Map.null m
      then encCBOR c
      else
        encode $
          Rec MaryValue
            !> To c
            !> To ma

instance Crypto c => DecCBOR (MaryValue c) where
  decCBOR = decodeValue

-- Note: we do not use `decodeInt64` from the cborg library here because the
-- implementation contains "-- TODO FIXME: overflow"
decodeIntegerBounded64 :: Decoder s Integer
decodeIntegerBounded64 = do
  tt <- peekTokenType
  case tt of
    TypeUInt -> pure ()
    TypeUInt64 -> pure ()
    TypeNInt -> pure ()
    TypeNInt64 -> pure ()
    _ -> fail "expected major type 0 or 1 when decoding mint field"
  x <- decodeInteger
  if minval <= x && x <= maxval
    then
      if x == 0
        then ifDecoderVersionAtLeast (natVersion @9) (fail "mint field cannot be 0 (zero)") (pure x)
        else pure x
    else
      fail $
        concat
          [ "overflow when decoding mint field. min value: "
          , show minval
          , " max value: "
          , show maxval
          , " got: "
          , show x
          ]
  where
    maxval = fromIntegral (maxBound :: Int64)
    minval = fromIntegral (minBound :: Int64)

-- ========================================================================
-- JSON

instance Crypto c => ToJSON (MaryValue c) where
  toJSON = object . toMaryValuePairs
  toEncoding = Aeson.pairs . mconcat . toMaryValuePairs

toMaryValuePairs :: Crypto c => Aeson.KeyValue a => MaryValue c -> [a]
toMaryValuePairs (MaryValue l ps) =
  [ "lovelace" .= l
  , "policies" .= ps
  ]

instance ToJSON AssetName where
  toJSON = Aeson.String . assetNameToTextAsHex

instance ToJSONKey AssetName where
  toJSONKey = toJSONKeyText assetNameToTextAsHex

-- ========================================================================
-- Compactible
-- This is used in the TxOut which stores the (CompactForm MaryValue).

instance Crypto c => Compactible (MaryValue c) where
  newtype CompactForm (MaryValue c) = CompactValue (CompactValue c)
    deriving (Eq, Typeable, Show, NoThunks, EncCBOR, DecCBOR, NFData, ToExpr)
  toCompact x = CompactValue <$> to x
  fromCompact (CompactValue x) = from x

instance Crypto c => EncCBOR (CompactValue c) where
  encCBOR = encCBOR . from

instance Crypto c => DecCBOR (CompactValue c) where
  decCBOR = do
    v <- decodeValue
    case to v of
      Nothing ->
        fail
          "impossible failure: decoded nonnegative value that cannot be compacted"
      Just x -> pure x

data CompactValue c
  = CompactValueAdaOnly {-# UNPACK #-} !(CompactForm Coin)
  | CompactValueMultiAsset
      {-# UNPACK #-} !(CompactForm Coin) -- ada
      {-# UNPACK #-} !Word32 -- number of ma's
      {-# UNPACK #-} !ShortByteString -- rep
  deriving (Generic, Show, Typeable)

instance NFData (CompactValue c) where
  rnf = rwhnf

instance Crypto c => Eq (CompactValue c) where
  a == b = from a == from b

instance ToExpr (CompactValue c)

deriving via
  OnlyCheckWhnfNamed "CompactValue" (CompactValue c)
  instance
    NoThunks (CompactValue c)

{-
The MaryValue surface type uses a nested map. For the compact version we use a
flattened representation, equivalent to a list of triples:
  [(PolicyID, AssetName, Quantity)]

Example:
  [ ("0xa519f84e...", "",       42)  -- empty asset name
  , ("0xf820a82c...", "Snark",  1)
  , ("0xf820a82c...", "Boojum", 1)   -- shared policy id, different name
  ]

We start by sorting in /descending/ order by asset name. Note that descending
order puts empty strings last:
  [ ("0xf820a82c...", "Snark",  1)
  , ("0xf820a82c...", "Boojum", 1)
  , ("0xa519f84e...", "",      42)
  ]

This example will be serialised as:
  ┏━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━┓
A)┃             1 ┃             1 ┃            42 ┃ Word64 quantities
  ┣━━━┳━━━┳━━━┳━━━┻━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━┛
B)┃ 36┃ 36┃ 64┃                                     Word16 policyId offsets
  ┣━━━╋━━━╋━━━┫
C)┃ 92┃ 98┃103┃                                     Word16 asset name offsets
  ┣━┯━╇━┯━╇━┯━╇━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┯━┓
D)┃f820a82c.. ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┃ 28 byte policyId #1
  ┣━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━┫
  ┃a519f84e.. ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┊ ┃ 28 byte policyId #2
  ┣━┿━┿━┿━┿━┿━┿━┿━┿━┿━┿━╈━╈━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┛
E)┃S┊n┊a┊r┊k┊B┊o┊o┊j┊u┊m┃◀╂─padding to word size    Asset names, plus padding
  ┗━┷━┷━┷━┷━┷━┷━┷━┷━┷━┷━┻━┛
   ▲         ▲           ▲
   92-offset 97-offset   103-offset, end of string

Note that the asset name offsets end up sorted in ascending order, with empty
asset names pointing to the offset past the end of the concatenated names.

The serialised representation consists of five parts, concatenated together:
  A) A sequence of Word64s representing asset quantities.

  B) A sequence of Word16s representing policyId string offsets within D.
     We do not need to store a length because policyIds are fixed size.

  C) A sequence of Word16s representing asset name string offsets within E.
     We store only the starting offset and not a end offset or length. We can
     do this because we keep the assets in A,B,C sorted by the asset name, so
     the offsets in C are sorted. This means the next distinct entry gives the
     (exclusive) end offset of the asset name string. As a special case for
     empty asset names, the index points to the end of the string E.

     Note: If there are duplicate asset names, this can yield a sequence of
     multiple of the same offset. For example, if the assets are named
     "Snark","Snark","Boojum", region E would contain "SnarkBoojum",
     and region C would contain 92, 92, 97. For the decoder to determine the
     length of the first asset, it would subtract 92 from 97 (and not from the
     duplicate 92).

  D) a blob of policyIDs, without duplicates, concatenated.

  E) a blob of asset names, sorted, without duplicates, concatenated.

The size of the regions A,B,C are known based on the number of values. The
string offsets in B and C are relative to the whole of the representation, not
relative to the start of D & E (since D is variable size depending on whether
there were duplicate policyIDs)

The encoding strategy is
 - Collect all (unique) policy Ids.
 - Collect all (unique) asset names.
 - Determine the sizes of the regions and allocate them.
   - size A = 8 * numassets
   - size B = 2 * numassets
   - size C = 2 * numassets
   - size D = length (concat policyIds)
   - size E = length (concat assetNames)
   - sum = 12*numassets
         + length (concat policyIds)
         + length (concat assetNames)
 - Write the policyIds to region D
 - Write the asset names to region E
 - For each asset entry
   - Locate the corresponding asset name and policyId
   - Write quantity, policyId location, and asset name location to corresponding
     regions
   - For the special case of 0 length asset names, the location is the end of
     region E

The decoding strategy is
 - Use length information to determine the beginnings of regions A,B,C
   (We do not need to do this for regions D and E because the policyId
   and asset name locations are relative to the beginning of entire rep.)
 - For each integer in 0..(numassets -1)
   - Read corresponding quantity, pid offset, and asset name offset from regions
     A, B, and C, respectively.
   - Read (pid length) bytes from pid offset. assume it points into region D
   - Determine asset name lengths using the difference between the offset and
     the next greater offset (if it exists). If there is no next greater offset,
     use the difference from the end of the rep. (Note: for the special case of
     0 length asset names, this calculation results in 0 because we are
     subtracting the end of the rep from itself.)
   - Read (asset name length) bytes from asset name offset. assume it points
     into region E.
 -}

to ::
  forall c.
  Crypto c =>
  MaryValue c ->
  -- The Nothing case of the return value corresponds to a quantity that is outside
  -- the bounds of a Word64. x < 0 or x > (2^64 - 1)
  Maybe (CompactValue c)
to (MaryValue ada (MultiAsset m))
  | Map.null m =
      CompactValueAdaOnly . CompactCoin <$> integerToWord64 ada
to v@(MaryValue _ ma) = do
  c <- assert (isMultiAssetSmallEnough ma) (integerToWord64 ada)
  -- Here we convert the (pid, assetName, quantity) triples into
  -- (Int, (Word16,Word16,Word64))
  -- These represent the index, pid offset, asset name offset, and quantity.
  -- If any of the quantities out of bounds, this will produce Nothing.
  -- The triples are ordered by asset name in descending order.
  preparedTriples <-
    zip [0 ..] . sortOn (\(_, x, _) -> x) <$> traverse prepare triples
  pure $
    CompactValueMultiAsset (CompactCoin c) (fromIntegral numTriples) $
      runST $ do
        byteArray <- BA.newByteArray repSize
        forM_ preparedTriples $ \(i, (pidoff, anoff, q)) ->
          do
            -- For each triple, we write the quantity to region A,
            -- the pid offset to region B, and the asset name offset to region C.
            -- We can calculate the sizes (and therefore the starts) of each region
            -- using the number of triples.
            -- A:
            --   size: (#triples * 8) bytes
            --   start: offset 0
            -- B:
            --   size: (#triples * 2) bytes
            --   start: size(A) = #triples * 8
            -- C:
            --   size: (#triples * 2) bytes
            --   start: size(A) + size(B) = #triples * 10
            --
            -- The position argument to writeByteArray is an index in terms of the
            -- size of the value being written. So writeByteArray of a Word64 at
            -- position 1 writes at offset 8.
            --
            -- For the following, the byte offsets calculated above are converted to
            -- ByteArray positions by division.
            --
            -- The byte offset of the ith...
            --   quantity: 8i
            --   pid offset: 8n + 2i
            --   asset name offset: 8n + 2n + 2i
            -- Dividing by the respective sizes (8,2,2) yields the indices below.
            BA.writeByteArray byteArray i q
            BA.writeByteArray byteArray (4 * numTriples + i) pidoff
            BA.writeByteArray byteArray (5 * numTriples + i) anoff

        forM_ (Map.toList pidOffsetMap) $
          \(PolicyID (ScriptHash sh), offset) ->
            let pidBytes = Hash.hashToBytesShort sh
             in BA.copyByteArray
                  byteArray
                  (fromIntegral offset)
                  (sbsToByteArray pidBytes)
                  0
                  pidSize

        forM_ (Map.toList assetNameOffsetMap) $
          \(AssetName anameBS, offset) ->
            let anameBytes = anameBS
                anameLen = SBS.length anameBS
             in BA.copyByteArray
                  byteArray
                  (fromIntegral offset)
                  (sbsToByteArray anameBytes)
                  0
                  anameLen
        byteArrayToSbs <$> BA.unsafeFreezeByteArray byteArray
  where
    (ada, triples) = gettriples v
    numTriples = length triples

    -- abcRegionSize is the combined size of regions A, B, and C
    abcRegionSize = numTriples * 12

    pidSize = fromIntegral (Hash.sizeHash (Proxy :: Proxy (ADDRHASH c)))

    -- pids is the collection of all distinct pids
    pids = Set.fromList $ (\(pid, _, _) -> pid) <$> triples

    pidOffsetMap :: Map (PolicyID c) Word16
    pidOffsetMap =
      -- the pid offsets are:
      --   X, X + s, X + 2s, X + 3s, ...
      -- where X is the start of block D and s is the size of a pid
      let offsets =
            enumFromThen (fromIntegral abcRegionSize) (fromIntegral (abcRegionSize + pidSize))
       in Map.fromList (zip (Set.toList pids) offsets)

    pidOffset pid = fromJust (Map.lookup pid pidOffsetMap)

    pidBlockSize = Set.size pids * pidSize

    -- Putting asset names in descending order ensures that the empty string
    -- is last, so the associated offset is pointing to the end of the array
    assetNames = Set.toDescList $ Set.fromList $ (\(_, an, _) -> an) <$> triples

    assetNameLengths = fromIntegral . SBS.length . assetName <$> assetNames

    assetNameOffsetMap :: Map AssetName Word16
    assetNameOffsetMap =
      -- The asset name offsets are the running sum of the asset lengths,
      -- but starting with the offset of the start of block E.
      let offsets = scanl (+) (abcRegionSize + pidBlockSize) assetNameLengths
       in fromIntegral <$> Map.fromList (zip assetNames offsets)

    assetNameOffset aname = fromJust (Map.lookup aname assetNameOffsetMap)

    anameBlockSize = sum assetNameLengths

    -- size = size(A+B+C)      + size(D)      + size(E)
    repSize = abcRegionSize + pidBlockSize + anameBlockSize

    prepare (pid, aname, q) = do
      q' <- integerToWord64 q
      pure (pidOffset pid, assetNameOffset aname, q')

-- | Unlike `representationSize`, this function cheaply checks whether
-- any offset within a MultiAsset compact representation is likely to overflow Word16.
--
-- compact form inequality:
--   8n (Word64) + 2n (Word16) + 2n (Word16) + 28p (policy ids) + sum of lengths of unique asset names <= 65535
-- maximum size for the asset name is 32 bytes, so:
-- 8n + 2n + 2n + 28p + 32n <= 65535
-- where: n = total number of assets, p = number of unique policy ids
isMultiAssetSmallEnough :: MultiAsset c -> Bool
isMultiAssetSmallEnough (MultiAsset ma) =
  44 * M.getSum (foldMap' (M.Sum . length) ma) + 28 * length ma <= 65535

representationSize ::
  forall c.
  Crypto c =>
  [(PolicyID c, AssetName, Integer)] ->
  Int
representationSize xs = abcRegionSize + pidBlockSize + anameBlockSize
  where
    len = length xs
    abcRegionSize = len * 12

    numPids = Set.size . Set.fromList $ (\(pid, _, _) -> pid) <$> xs
    pidSize = fromIntegral (Hash.sizeHash (Proxy :: Proxy (ADDRHASH c)))
    pidBlockSize = numPids * pidSize

    assetNames = Set.fromList $ (\(_, an, _) -> an) <$> xs
    anameBlockSize =
      Semigroup.getSum $ foldMap' (Semigroup.Sum . SBS.length . assetName) assetNames

from :: forall c. Crypto c => CompactValue c -> MaryValue c
from (CompactValueAdaOnly (CompactCoin c)) = MaryValue (fromIntegral c) (MultiAsset Map.empty)
from (CompactValueMultiAsset (CompactCoin c) numAssets rep) =
  let mv@(MaryValue _ ma) = valueFromList (fromIntegral c) triples
   in assert (isMultiAssetSmallEnough ma) mv
  where
    n = fromIntegral numAssets

    ba = sbsToByteArray rep

    getTripleForIndex :: Int -> (Word16, Word16, Word64)
    getTripleForIndex i =
      -- indexByteArray indices are in terms of the size of the value being indexed
      -- rather than byte offsets.
      -- The corresponding byte offsets are:
      -- q: 0 + 8i
      -- pidix: 8n + 2i
      -- anameix: 8n + 2n + 2i
      -- Dividing by the sized (resp 8,2,2) yields the indices below
      let q = BA.indexByteArray ba i
          pidix = BA.indexByteArray ba (4 * n + i)
          anameix = BA.indexByteArray ba (5 * n + i)
       in (pidix, anameix, q)

    -- raw triples :: [(pid offset, asset name offset, quantity)]
    rawTriples :: [(Word16, Word16, Word64)]
    rawTriples = map getTripleForIndex [0 .. (fromIntegral $ numAssets - 1)]

    triples :: [(PolicyID c, AssetName, Integer)]
    triples = map convertTriple rawTriples

    -- Asset name length are calculated by subtracting the offset from the
    -- next greater offset (or from the end of the rep, if there is none.)
    -- For an index pointing to the end of the array, the associated
    -- length will be: offset - length(rep) = 0
    assetLens =
      -- This assumes that the triples are ordered by nondecreasing asset name offset
      let ixs = nubOrd $ map (\(_, x, _) -> x) rawTriples
          ixPairs = zip ixs (drop 1 ixs ++ [fromIntegral $ SBS.length rep])
       in Map.fromList $ (\(a, b) -> (a, fromIntegral $ b - a)) <$> ixPairs
    assetLen :: Word16 -> Int
    assetLen ix = fromJust (Map.lookup ix assetLens)

    convertTriple ::
      (Word16, Word16, Word64) -> (PolicyID c, AssetName, Integer)
    convertTriple (p, a, i) =
      ( PolicyID $
          ScriptHash $
            Hash.UnsafeHash $
              readShortByteString
                rep
                (fromIntegral p)
                (fromIntegral $ Hash.sizeHash ([] :: [ADDRHASH c]))
      , AssetName $ readShortByteString rep (fromIntegral a) (assetLen a)
      , fromIntegral i
      )

-- | Strip out duplicates
nubOrd :: Ord a => [a] -> [a]
nubOrd =
  loop mempty
  where
    loop _ [] = []
    loop s (a : as)
      | a `Set.member` s = loop s as
      | otherwise =
          let s' = Set.insert a s in s' `seq` a : loop s' as

sbsToByteArray :: ShortByteString -> BA.ByteArray
sbsToByteArray (SBS bah) = BA.ByteArray bah

byteArrayToSbs :: BA.ByteArray -> ShortByteString
byteArrayToSbs (BA.ByteArray bah) = SBS bah

readShortByteString :: ShortByteString -> Int -> Int -> ShortByteString
readShortByteString sbs start len =
  byteArrayToSbs $ BA.cloneByteArray (sbsToByteArray sbs) start len

-- ========================================================================
-- Operations on Values

-- | Extract the set of policies in the Value.
--
--   This function is equivalent to computing the support of the value in the
--   spec.
policies :: MultiAsset c -> Set (PolicyID c)
policies (MultiAsset m) = Map.keysSet m

lookup :: PolicyID c -> AssetName -> MaryValue c -> Integer
lookup = lookupMultiAsset
{-# DEPRECATED lookup "In favor of `lookupMultiAsset`" #-}

lookupMultiAsset :: PolicyID c -> AssetName -> MaryValue c -> Integer
lookupMultiAsset pid aid (MaryValue _ (MultiAsset m)) =
  case Map.lookup pid m of
    Nothing -> 0
    Just m2 -> Map.findWithDefault 0 aid m2

-- | insert comb policy asset n v,
--   if comb = \ old new -> old, the integer in the MultiAsset is prefered over n
--   if comb = \ old new -> new, then n is prefered over the integer in the MultiAsset
--   if (comb old new) == 0, then that value should not be stored in the MultiAsset
insert ::
  (Integer -> Integer -> Integer) ->
  PolicyID c ->
  AssetName ->
  Integer ->
  MultiAsset c ->
  MultiAsset c
insert = insertMultiAsset
{-# DEPRECATED insert "In favor of `insertMultiAsset`" #-}

-- | insertMultiAsset comb policy asset n v,
--   if comb = \ old new -> old, the integer in the MultiAsset is prefered over n
--   if comb = \ old new -> new, then n is prefered over the integer in the MultiAsset
--   if (comb old new) == 0, then that value should not be stored in the MultiAsset
insertMultiAsset ::
  (Integer -> Integer -> Integer) ->
  PolicyID c ->
  AssetName ->
  Integer ->
  MultiAsset c ->
  MultiAsset c
insertMultiAsset combine pid aid new (MultiAsset m1) =
  case Map.splitLookup pid m1 of
    (l1, Just m2, l2) ->
      case Map.splitLookup aid m2 of
        (v1, Just old, v2) ->
          if n == 0
            then
              let m3 = link2 v1 v2
               in if Map.null m3
                    then MultiAsset (link2 l1 l2)
                    else MultiAsset (link pid m3 l1 l2)
            else MultiAsset (link pid (link aid n v1 v2) l1 l2)
          where
            n = combine old new
        (_, Nothing, _) ->
          MultiAsset
            ( link
                pid
                ( if new == 0
                    then m2
                    else Map.insert aid new m2
                )
                l1
                l2
            )
    (l1, Nothing, l2) ->
      MultiAsset
        ( if new == 0
            then link2 l1 l2
            else link pid (Map.singleton aid new) l1 l2
        )

-- ========================================================

-- | Remove 0 assets from a map
prune ::
  Map (PolicyID c) (Map AssetName Integer) ->
  Map (PolicyID c) (Map AssetName Integer)
prune assets =
  Map.filter (not . null) $ Map.filter (/= 0) <$> assets

-- | Rather than using prune to remove 0 assets, when can avoid adding them in the
--   first place by using valueFromList to construct a MultiAsset
multiAssetFromList :: [(PolicyID era, AssetName, Integer)] -> MultiAsset era
multiAssetFromList = foldr (\(p, n, i) ans -> insertMultiAsset (+) p n i ans) mempty

valueFromList :: Integer -> [(PolicyID era, AssetName, Integer)] -> MaryValue era
valueFromList ada triples = MaryValue ada (multiAssetFromList triples)

-- | Display a MaryValue as a String, one token per line
showValue :: MaryValue c -> String
showValue v = show c ++ "\n" ++ unlines (map trans ts)
  where
    (c, ts) = gettriples v
    trans (PolicyID x, hash, cnt) =
      show x
        ++ ",  "
        ++ show hash
        ++ ",  "
        ++ show cnt

-- | Turn the nested 'MaryValue' map-of-maps representation into a flat sequence
-- of policyID, asset name and quantity, plus separately the ada quantity.
gettriples :: MaryValue c -> (Integer, [(PolicyID c, AssetName, Integer)])
gettriples (MaryValue c ma) = (c, flattenMultiAsset ma)

flattenMultiAsset :: MultiAsset c -> [(PolicyID c, AssetName, Integer)]
flattenMultiAsset (MultiAsset m) =
  [ (policyId, aname, amount)
  | (policyId, m2) <- assocs m
  , (aname, amount) <- assocs m2
  ]

-- ==========================================

instance ToExpr (MaryValue c)

instance ToExpr (MultiAsset c)

instance ToExpr (PolicyID c)

instance ToExpr AssetName where
  toExpr an = App "AssetName" [toExpr (assetNameToTextAsHex an)]
