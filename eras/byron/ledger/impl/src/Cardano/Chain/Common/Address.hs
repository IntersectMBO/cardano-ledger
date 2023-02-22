{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functionality related to 'Address' data type and related types.
module Cardano.Chain.Common.Address (
  Address (..),
  Address' (..),

  -- * Formatting
  addressF,
  addressDetailedF,
  decCBORTextAddress,

  -- * Spending data checks
  checkAddrSpendingData,
  checkVerKeyAddress,
  checkRedeemAddress,

  -- * Encoding/Decoding
  addrToBase58,
  encCBORAddr,
  encCBORAddrCRC32,
  decodeAddressBase58,
  encodeAddressBase58,

  -- * Utilities
  addrAttributesUnwrapped,
  addrNetworkMagic,

  -- * Pattern-matching helpers
  isRedeemAddress,

  -- * Construction
  makeAddress,
  makeVerKeyAddress,
  makeVerKeyHdwAddress,
  makeRedeemAddress,
)
where

import Cardano.Chain.Common.AddrAttributes (
  AddrAttributes (..),
  HDAddressPayload,
 )
import Cardano.Chain.Common.AddrSpendingData (
  AddrSpendingData (..),
  AddrType (..),
  addrSpendingDataToType,
 )
import Cardano.Chain.Common.AddressHash (AddressHash, addressHash)
import Cardano.Chain.Common.Attributes (Attributes (..), mkAttributes)
import Cardano.Chain.Common.CBOR (
  decodeCrcProtected,
  encodeCrcProtected,
  encodedCrcProtectedSizeExpr,
 )
import Cardano.Chain.Common.NetworkMagic (NetworkMagic (..))
import Cardano.Crypto.Hashing (hashHexF)
import Cardano.Crypto.Signing (
  RedeemVerificationKey,
  VerificationKey,
 )
import Cardano.HeapWords (HeapWords (..), heapWords3)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecoderError (..),
  EncCBOR (..),
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  byronProtVer,
  decodeFull',
  decodeListLenCanonical,
  fromByronCBOR,
  matchSize,
  serialize',
  toByronCBOR,
 )
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Data.ByteString.Base58 (
  Alphabet (..),
  bitcoinAlphabet,
  decodeBase58,
  encodeBase58,
 )
import Data.Text.Encoding (decodeLatin1)
import Data.Text.Internal.Builder (Builder)
import Formatting (
  Format,
  bprint,
  build,
  builder,
  formatToString,
  later,
 )
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))
import Text.JSON.Canonical (
  FromJSON (..),
  FromObjectKey (..),
  JSValue (..),
  ToJSON (..),
  ToObjectKey (..),
  toJSString,
 )

-- | Hash of this data is stored in 'Address'. This type exists mostly
--   for internal usage.
newtype Address' = Address'
  { unAddress' :: (AddrType, AddrSpendingData, Attributes AddrAttributes)
  }
  deriving (Eq, Show, Generic)
  deriving newtype (EncCBOR)

instance ToCBOR Address' where
  toCBOR = toByronCBOR

instance FromCBOR Address' where
  fromCBOR = fromByronCBOR

-- We need to use canonical encodings for @Address'@ so that all implementations
-- agree on the `AddressHash`. The components of the @Address'@ also have
-- canonical encodings enforced.
instance DecCBOR Address' where
  decCBOR = do
    len <- decodeListLenCanonical
    matchSize "Address'" 3 len
    fmap Address' $ (,,) <$> decCBOR <*> decCBOR <*> decCBOR

-- | 'Address' is where you can send Lovelace
data Address = Address
  { addrRoot :: !(AddressHash Address')
  -- ^ Root of imaginary pseudo Merkle tree stored in this address.
  , addrAttributes :: !(Attributes AddrAttributes)
  -- ^ Attributes associated with this address.
  , addrType :: !AddrType
  -- ^ The type of this address. Should correspond to
  -- 'AddrSpendingData', but it can't be checked statically, because
  -- spending data is hashed.
  }
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance Aeson.ToJSON Address

instance ToCBOR Address where
  toCBOR = toByronCBOR

instance FromCBOR Address where
  fromCBOR = fromByronCBOR

instance EncCBOR Address where
  encCBOR addr =
    encodeCrcProtected (addrRoot addr, addrAttributes addr, addrType addr)

  encodedSizeExpr size pxy =
    encodedCrcProtectedSizeExpr size $
      (,,)
        <$> (addrRoot <$> pxy)
        <*> (addrAttributes <$> pxy)
        <*> (addrType <$> pxy)

instance DecCBOR Address where
  decCBOR = do
    (root, attributes, addrType') <- decodeCrcProtected
    pure $
      Address
        { addrRoot = root
        , addrAttributes = attributes
        , addrType = addrType'
        }

instance B.Buildable [Address] where
  build = bprint listJson

instance Monad m => ToObjectKey m Address where
  toObjectKey = pure . toJSString . formatToString addressF

instance MonadError SchemaError m => FromObjectKey m Address where
  fromObjectKey = fmap Just . parseJSString decCBORTextAddress . JSString

instance Monad m => ToJSON m Address where
  toJSON = fmap JSString . toObjectKey

instance MonadError SchemaError m => FromJSON m Address where
  fromJSON = parseJSString decCBORTextAddress

instance HeapWords Address where
  heapWords (Address root attrs typ) = heapWords3 root attrs typ

--------------------------------------------------------------------------------
-- Formatting, pretty-printing
--------------------------------------------------------------------------------

-- | A formatter showing guts of an 'Address'
addressDetailedF :: Format r (Address -> r)
addressDetailedF = later $ \addr ->
  bprint
    (builder . " address with root " . hashHexF . ", attributes: " . build)
    (formattedType $ addrType addr)
    (addrRoot addr)
    (addrAttributes addr)
  where
    formattedType :: AddrType -> Builder
    formattedType = \case
      ATVerKey -> "VerKey"
      ATRedeem -> "Redeem"

-- | Currently we use Bitcoin alphabet for representing addresses in base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . serialize' byronProtVer

instance B.Buildable Address where
  build = B.build . decodeLatin1 . addrToBase58

-- | Specialized formatter for 'Address'
addressF :: Format r (Address -> r)
addressF = build

-- | A function which decodes base58-encoded 'Address'
{-# DEPRECATED decCBORTextAddress "Use decodeAddressBase58 instead" #-}
decCBORTextAddress :: Text -> Either DecoderError Address
decCBORTextAddress = decCBORAddress . encodeUtf8
  where
    decCBORAddress :: ByteString -> Either DecoderError Address
    decCBORAddress bs = do
      let base58Err =
            DecoderErrorCustom
              "Address"
              "Invalid base58 representation of address"
      dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
      decodeFull' byronProtVer dbs

-- | Decode an address from Base58 encoded Text.
decodeAddressBase58 :: Text -> Either DecoderError Address
decodeAddressBase58 = decCBORTextAddress

-- | Encode an address to Text.
-- `decodeAddressBase58 (encodeAddressBase58 x) === Right x`
encodeAddressBase58 :: Address -> Text
encodeAddressBase58 = decodeLatin1 . addrToBase58

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

-- | Make an 'Address' from spending data and attributes.
makeAddress :: AddrSpendingData -> AddrAttributes -> Address
makeAddress spendingData attributesUnwrapped =
  Address
    { addrRoot = addressHash address'
    , addrAttributes = attributes
    , addrType = addrType'
    }
  where
    addrType' = addrSpendingDataToType spendingData
    attributes = mkAttributes attributesUnwrapped
    address' = Address' (addrType', spendingData, attributes)

-- | A function for making an address from 'VerificationKey'
makeVerKeyAddress :: NetworkMagic -> VerificationKey -> Address
makeVerKeyAddress nm = makeVerKeyAddressImpl nm Nothing

-- | A function for making an HDW address
makeVerKeyHdwAddress ::
  NetworkMagic ->
  -- | Derivation path
  HDAddressPayload ->
  VerificationKey ->
  Address
makeVerKeyHdwAddress nm path = makeVerKeyAddressImpl nm (Just path)

makeVerKeyAddressImpl :: NetworkMagic -> Maybe HDAddressPayload -> VerificationKey -> Address
makeVerKeyAddressImpl nm path key = makeAddress spendingData attrs
  where
    spendingData = VerKeyASD key
    attrs =
      AddrAttributes
        { aaVKDerivationPath = path
        , aaNetworkMagic = nm
        }

-- | A function for making an address from 'RedeemVerificationKey'
makeRedeemAddress :: NetworkMagic -> RedeemVerificationKey -> Address
makeRedeemAddress nm key = makeAddress spendingData attrs
  where
    spendingData = RedeemASD key
    attrs =
      AddrAttributes
        { aaVKDerivationPath = Nothing
        , aaNetworkMagic = nm
        }

--------------------------------------------------------------------------------
-- Checks
--------------------------------------------------------------------------------

-- | Check whether given 'AddrSpendingData' corresponds to given 'Address'
checkAddrSpendingData :: AddrSpendingData -> Address -> Bool
checkAddrSpendingData asd addr =
  addrRoot addr
    == addressHash address'
    && addrType addr
      == addrSpendingDataToType asd
  where
    address' = Address' (addrType addr, asd, addrAttributes addr)

-- | Check if given 'Address' is created from given 'VerificationKey'
checkVerKeyAddress :: VerificationKey -> Address -> Bool
checkVerKeyAddress vk = checkAddrSpendingData (VerKeyASD vk)

-- | Check if given 'Address' is created from given 'RedeemVerificationKey'
checkRedeemAddress :: RedeemVerificationKey -> Address -> Bool
checkRedeemAddress rvk = checkAddrSpendingData (RedeemASD rvk)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Get 'AddrAttributes' from 'Address'
addrAttributesUnwrapped :: Address -> AddrAttributes
addrAttributesUnwrapped = attrData . addrAttributes

-- | Get 'NetworkMagic' from 'Address'
addrNetworkMagic :: Address -> NetworkMagic
addrNetworkMagic = aaNetworkMagic . addrAttributesUnwrapped

--------------------------------------------------------------------------------
-- Pattern-matching helpers
--------------------------------------------------------------------------------

-- | Check whether an 'Address' is redeem address
isRedeemAddress :: Address -> Bool
isRedeemAddress addr = case addrType addr of
  ATRedeem -> True
  _ -> False

-- Encodes the `Address` __without__ the CRC32.
-- It's important to keep this function separated from the `encCBOR`
-- definition to avoid that `encCBOR` would call `crc32` and
-- the latter invoke `crc32Update`, which would then try to call `encCBOR`
-- indirectly once again, in an infinite loop.
encCBORAddr :: Address -> Encoding
encCBORAddr addr =
  encCBOR (addrRoot addr)
    <> encCBOR (addrAttributes addr)
    <> encCBOR
      (addrType addr)

encCBORAddrCRC32 :: Address -> Encoding
encCBORAddrCRC32 addr =
  encodeCrcProtected (addrRoot addr, addrAttributes addr, addrType addr)
