{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Functionality related to 'Address' data type and related types.

module Cardano.Chain.Common.Address
  ( Address(..)
  , Address'(..)

  -- * Formatting
  , addressF
  , addressDetailedF
  , fromCBORTextAddress

  -- * Spending data checks
  , checkAddrSpendingData
  , checkVerKeyAddress
  , checkRedeemAddress

  -- * Encoding
  , addrToBase58
  , toCBORAddr
  , toCBORAddrCRC32

  -- * Utilities
  , addrAttributesUnwrapped
  , addrNetworkMagic
  , deriveLvl2KeyPair
  , deriveFirstHDAddress
  , unAddressHash

  -- * Pattern-matching helpers
  , isRedeemAddress

  -- * Construction
  , makeAddress
  , makeVerKeyAddress
  , makeVerKeyHdwAddress
  , makeRedeemAddress
  , createHDAddressNH
  , createHDAddressH
  )
where

import Cardano.Prelude

import qualified Data.ByteArray

import Control.Monad.Except (MonadError)
import Data.ByteString.Base58
  (Alphabet(..), bitcoinAlphabet, decodeBase58, encodeBase58)
import Data.Text.Internal.Builder (Builder)
import Formatting
  (Format, bprint, build, builder, formatToString, later)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical
  ( FromJSON(..)
  , FromObjectKey(..)
  , JSValue(..)
  , ToJSON(..)
  , ToObjectKey(..)
  , toJSString
  )

import Cardano.Binary
  ( DecoderError(..)
  , Encoding
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeFull'
  , decodeListLenCanonical
  , matchSize
  , serialize'
  )
import Cardano.Chain.Common.CBOR
  (encodeCrcProtected, encodedCrcProtectedSizeExpr, decodeCrcProtected)
import Cardano.Chain.Common.AddrAttributes (AddrAttributes(..))
import Cardano.Chain.Common.AddressHash (AddressHash, addressHash)
import Cardano.Chain.Common.AddrSpendingData
  (AddrSpendingData(..), AddrType(..), addrSpendingDataToType)
import Cardano.Chain.Common.Attributes (Attributes(..), mkAttributes)
import Cardano.Chain.Common.NetworkMagic (NetworkMagic(..))
import Cardano.Chain.Constants (accountGenesisIndex, wAddressGenesisIndex)
import Cardano.Crypto.Hashing (hashHexF)
import Cardano.Crypto.HD
  ( HDAddressPayload
  , HDPassphrase
  , ShouldCheckPassphrase(..)
  , deriveHDPassphrase
  , deriveHDVerificationKey
  , deriveHDSigningKey
  , packHDAddressAttr
  )
import Cardano.Crypto.Signing
  ( EncryptedSigningKey
  , PassPhrase
  , VerificationKey
  , RedeemVerificationKey
  , encToVerification
  )


-- | Hash of this data is stored in 'Address'. This type exists mostly
--   for internal usage.
newtype Address' = Address'
  { unAddress' :: (AddrType, AddrSpendingData, Attributes AddrAttributes)
  } deriving (Eq, Show, Generic)
    deriving newtype ToCBOR

-- We need to use canonical encodings for @Address'@ so that all implementations
-- agree on the `AddressHash`. The components of the @Address'@ also have
-- canonical encodings enforced.
instance FromCBOR Address' where
  fromCBOR = do
    len <- decodeListLenCanonical
    matchSize "Address'" 3 len
    fmap Address' $ (,,) <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Get the ByteString of the hash.
unAddressHash :: AddressHash Address' -> ByteString
unAddressHash = Data.ByteArray.convert

-- | 'Address' is where you can send Lovelace
data Address = Address
  { addrRoot       :: !(AddressHash Address')
  -- ^ Root of imaginary pseudo Merkle tree stored in this address.
  , addrAttributes :: !(Attributes AddrAttributes)
  -- ^ Attributes associated with this address.
  , addrType       :: !AddrType
  -- ^ The type of this address. Should correspond to
  -- 'AddrSpendingData', but it can't be checked statically, because
  -- spending data is hashed.
  } deriving (Eq, Ord, Generic, Show)
    deriving anyclass NFData

instance ToCBOR Address where
  toCBOR addr =
    encodeCrcProtected (addrRoot addr, addrAttributes addr, addrType addr)

  encodedSizeExpr size pxy =
    encodedCrcProtectedSizeExpr size
      $   (,,)
      <$> (addrRoot <$> pxy)
      <*> (addrAttributes <$> pxy)
      <*> (addrType <$> pxy)

instance FromCBOR Address where
  fromCBOR = do
    (root, attributes, addrType') <- decodeCrcProtected
    pure $ Address
      { addrRoot       = root
      , addrAttributes = attributes
      , addrType       = addrType'
      }

instance B.Buildable [Address] where
  build = bprint listJson

instance Monad m => ToObjectKey m Address where
  toObjectKey = pure . toJSString . formatToString addressF

instance MonadError SchemaError m => FromObjectKey m Address where
  fromObjectKey = fmap Just . parseJSString fromCBORTextAddress . JSString

instance Monad m => ToJSON m Address where
  toJSON = fmap JSString . toObjectKey

instance MonadError SchemaError m => FromJSON m Address where
  fromJSON = parseJSString fromCBORTextAddress

instance HeapWords Address where
  heapWords (Address root attrs typ) = heapWords3 root attrs typ


--------------------------------------------------------------------------------
-- Formatting, pretty-printing
--------------------------------------------------------------------------------

-- | A formatter showing guts of an 'Address'
addressDetailedF :: Format r (Address -> r)
addressDetailedF = later $ \addr -> bprint
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
addrToBase58 = encodeBase58 addrAlphabet . serialize'

instance B.Buildable Address where
  build = B.build . decodeUtf8 . addrToBase58

-- | Specialized formatter for 'Address'
addressF :: Format r (Address -> r)
addressF = build

-- | A function which decodes base58-encoded 'Address'
fromCBORTextAddress :: Text -> Either DecoderError Address
fromCBORTextAddress = fromCBORAddress . encodeUtf8
 where
  fromCBORAddress :: ByteString -> Either DecoderError Address
  fromCBORAddress bs = do
    let
      base58Err = DecoderErrorCustom
        "Address"
        "Invalid base58 representation of address"
    dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
    decodeFull' dbs

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

-- | Make an 'Address' from spending data and attributes.
makeAddress :: AddrSpendingData -> AddrAttributes -> Address
makeAddress spendingData attributesUnwrapped = Address
  { addrRoot       = addressHash address'
  , addrAttributes = attributes
  , addrType       = addrType'
  }
 where
  addrType'  = addrSpendingDataToType spendingData
  attributes = mkAttributes attributesUnwrapped
  address'   = Address' (addrType', spendingData, attributes)

-- | A function for making an address from 'VerificationKey'
makeVerKeyAddress :: NetworkMagic -> VerificationKey -> Address
makeVerKeyAddress nm = makeVerKeyAddressImpl nm Nothing

-- | A function for making an HDW address
makeVerKeyHdwAddress
  :: NetworkMagic
  -> HDAddressPayload
  -- ^ Derivation path
  -> VerificationKey
  -> Address
makeVerKeyHdwAddress nm path = makeVerKeyAddressImpl nm (Just path)

makeVerKeyAddressImpl :: NetworkMagic -> Maybe HDAddressPayload -> VerificationKey -> Address
makeVerKeyAddressImpl nm path key = makeAddress spendingData attrs
 where
  spendingData = VerKeyASD key
  attrs        = AddrAttributes { aaVKDerivationPath = path
                                , aaNetworkMagic = nm }

-- | A function for making an address from 'RedeemVerificationKey'
makeRedeemAddress :: NetworkMagic -> RedeemVerificationKey -> Address
makeRedeemAddress nm key = makeAddress spendingData attrs
 where
  spendingData = RedeemASD key
  attrs        = AddrAttributes { aaVKDerivationPath = Nothing
                                , aaNetworkMagic = nm }

-- | Create address from signing key in hardened way
createHDAddressH
  :: NetworkMagic
  -> ShouldCheckPassphrase
  -> PassPhrase
  -> HDPassphrase
  -> EncryptedSigningKey
  -> [Word32]
  -> Word32
  -> Maybe (Address, EncryptedSigningKey)
createHDAddressH nm scp passphrase hdPassphrase parent parentPath childIndex = do
  derivedSK <- deriveHDSigningKey scp passphrase parent childIndex
  let
    addressPayload =
      packHDAddressAttr hdPassphrase $ parentPath ++ [childIndex]
  let vk = encToVerification derivedSK
  return (makeVerKeyHdwAddress nm addressPayload vk, derivedSK)

-- | Create address from verification key via non-hardened way
createHDAddressNH
  :: NetworkMagic -> HDPassphrase -> VerificationKey -> [Word32] -> Word32 -> (Address, VerificationKey)
createHDAddressNH nm passphrase parent parentPath childIndex = do
  let derivedVK = deriveHDVerificationKey parent childIndex
  let
    addressPayload = packHDAddressAttr passphrase $ parentPath ++ [childIndex]
  (makeVerKeyHdwAddress nm addressPayload derivedVK, derivedVK)


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
  where address' = Address' (addrType addr, asd, addrAttributes addr)

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

-- | Makes account signing key for given wallet set
deriveLvl2KeyPair
  :: NetworkMagic
  -> ShouldCheckPassphrase
  -> PassPhrase
  -> EncryptedSigningKey
  -- ^ key of wallet
  -> Word32
  -- ^ account derivation index
  -> Word32
  -- ^ address derivation index
  -> Maybe (Address, EncryptedSigningKey)
deriveLvl2KeyPair nm scp passphrase wsKey accountIndex addressIndex = do
  wKey <- deriveHDSigningKey scp passphrase wsKey accountIndex
  let hdPass = deriveHDPassphrase $ encToVerification wsKey
  -- We don't need to check passphrase twice
  createHDAddressH
    nm
    (ShouldCheckPassphrase False)
    passphrase
    hdPass
    wKey
    [accountIndex]
    addressIndex

deriveFirstHDAddress
  :: NetworkMagic
  -> PassPhrase
  -> EncryptedSigningKey
  -- ^ key of wallet set
  -> Maybe (Address, EncryptedSigningKey)
deriveFirstHDAddress nm passphrase wsKey = deriveLvl2KeyPair
  nm
  (ShouldCheckPassphrase False)
  passphrase
  wsKey
  accountGenesisIndex
  wAddressGenesisIndex


--------------------------------------------------------------------------------
-- Pattern-matching helpers
--------------------------------------------------------------------------------

-- | Check whether an 'Address' is redeem address
isRedeemAddress :: Address -> Bool
isRedeemAddress addr = case addrType addr of
  ATRedeem -> True
  _        -> False


-- Encodes the `Address` __without__ the CRC32.
-- It's important to keep this function separated from the `toCBOR`
-- definition to avoid that `toCBOR` would call `crc32` and
-- the latter invoke `crc32Update`, which would then try to call `toCBOR`
-- indirectly once again, in an infinite loop.
toCBORAddr :: Address -> Encoding
toCBORAddr addr =
  toCBOR (addrRoot addr) <> toCBOR (addrAttributes addr) <> toCBOR
    (addrType addr)

toCBORAddrCRC32 :: Address -> Encoding
toCBORAddrCRC32 addr =
  encodeCrcProtected (addrRoot addr, addrAttributes addr, addrType addr)
