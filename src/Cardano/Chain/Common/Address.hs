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
  , decodeTextAddress

  -- * Spending data checks
  , checkAddrSpendingData
  , checkPubKeyAddress
  , checkScriptAddress
  , checkRedeemAddress

  -- * Encoding
  , addrToBase58
  , encodeAddr
  , encodeAddrCRC32

  -- * Utilities
  , addrAttributesUnwrapped
  , deriveLvl2KeyPair
  , deriveFirstHDAddress

  -- * Pattern-matching helpers
  , isRedeemAddress
  , isUnknownAddressType

  -- * Construction
  , makeAddress
  , makePubKeyAddress
  , makePubKeyHdwAddress
  , makeScriptAddress
  , makeRedeemAddress
  , createHDAddressNH
  , createHDAddressH

  -- * Maximal sizes (needed for tx creation)
  , largestPubKeyAddress
  , maxPubKeyAddressSize
  , largestHDAddress
  , maxHDAddressSize
  )
where

import Cardano.Prelude

import Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson
  ( FromJSON(..)
  , FromJSONKey(..)
  , FromJSONKeyFunction(..)
  , ToJSON(toJSON)
  , ToJSONKey(..)
  )
import qualified Data.Aeson.Types as Aeson (toJSONKeyText)
import qualified Data.ByteString as BS
import Data.ByteString.Base58
  (Alphabet(..), bitcoinAlphabet, decodeBase58, encodeBase58)
import Formatting
  (Format, bprint, build, builder, formatToString, later, sformat)
import qualified Formatting.Buildable as B
import Text.JSON.Canonical
  (FromJSON(..), FromObjectKey(..), JSValue(..), ToJSON(..), ToObjectKey(..))

import Cardano.Binary.Class
  ( Bi(..)
  , DecoderError(..)
  , Encoding
  , biSize
  , encodeCrcProtected
  , encodedCrcProtectedSizeExpr
  )
import qualified Cardano.Binary.Class as Bi
import Cardano.Chain.Common.AddrAttributes (AddrAttributes(..))
import Cardano.Chain.Common.AddressHash (AddressHash, addressHash)
import Cardano.Chain.Common.AddrSpendingData
  (AddrSpendingData(..), AddrType(..), addrSpendingDataToType)
import Cardano.Chain.Common.Attributes (Attributes(..), mkAttributes)
import Cardano.Chain.Common.Script (Script)
import Cardano.Chain.Constants (accountGenesisIndex, wAddressGenesisIndex)
import Cardano.Crypto.Hashing (hashHexF)
import Cardano.Crypto.HD
  ( HDAddressPayload
  , HDPassphrase
  , ShouldCheckPassphrase(..)
  , deriveHDPassphrase
  , deriveHDPublicKey
  , deriveHDSecretKey
  , packHDAddressAttr
  )
import Cardano.Crypto.Signing
  ( EncryptedSecretKey
  , PassPhrase
  , PublicKey
  , RedeemPublicKey
  , SecretKey
  , deterministicKeyGen
  , emptyPassphrase
  , encToPublic
  , noPassEncrypt
  )


-- | Hash of this data is stored in 'Address'. This type exists mostly
--   for internal usage.
newtype Address' = Address'
  { unAddress' :: (AddrType, AddrSpendingData, Attributes AddrAttributes)
  } deriving (Eq, Show, Generic)
    deriving newtype Bi

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

instance Bi Address where
  encode addr =
    Bi.encodeCrcProtected (addrRoot addr, addrAttributes addr, addrType addr)

  decode = do
    (root, attributes, addrType') <- Bi.decodeCrcProtected
    pure $ Address
      { addrRoot       = root
      , addrAttributes = attributes
      , addrType       = addrType'
      }

  encodedSizeExpr size pxy =
    encodedCrcProtectedSizeExpr size
      $   (,,)
      <$> (addrRoot <$> pxy)
      <*> (addrAttributes <$> pxy)
      <*> (addrType <$> pxy)

instance B.Buildable [Address] where
  build = bprint listJson

instance Monad m => ToObjectKey m Address where
  toObjectKey = pure . formatToString addressF

instance MonadError SchemaError m => FromObjectKey m Address where
  fromObjectKey = fmap Just . parseJSString decodeTextAddress . JSString

instance Monad m => ToJSON m Address where
  toJSON = fmap JSString . toObjectKey

instance MonadError SchemaError m => FromJSON m Address where
  fromJSON = parseJSString decodeTextAddress

instance Aeson.FromJSONKey Address where
  fromJSONKey = Aeson.FromJSONKeyTextParser (toAesonError . decodeTextAddress)

instance Aeson.ToJSONKey Address where
  toJSONKey = Aeson.toJSONKeyText (sformat addressF)

instance Aeson.FromJSON Address where
  parseJSON = toAesonError . decodeTextAddress <=< Aeson.parseJSON

instance Aeson.ToJSON Address where
  toJSON = Aeson.toJSON . sformat addressF

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
  formattedType = \case
    ATPubKey      -> "PubKey"
    ATScript      -> "Script"
    ATRedeem      -> "Redeem"
    ATUnknown tag -> "Unknown#" <> B.build tag

-- | Currently we use Bitcoin alphabet for representing addresses in base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . Bi.serialize'

instance B.Buildable Address where
  build = B.build . decodeUtf8 . addrToBase58

-- | Specialized formatter for 'Address'
addressF :: Format r (Address -> r)
addressF = build

-- | A function which decodes base58-encoded 'Address'
decodeTextAddress :: Text -> Either DecoderError Address
decodeTextAddress = decodeAddress . encodeUtf8
 where
  decodeAddress :: ByteString -> Either DecoderError Address
  decodeAddress bs = do
    let
      base58Err = DecoderErrorCustom
        "Address"
        "Invalid base58 representation of address"
    dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
    Bi.decodeFull' dbs

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

-- | A function for making an address from 'PublicKey'
makePubKeyAddress :: PublicKey -> Address
makePubKeyAddress = makePubKeyAddressImpl Nothing

-- | A function for making an HDW address
makePubKeyHdwAddress
  :: HDAddressPayload
  -- ^ Derivation path
  -> PublicKey
  -> Address
makePubKeyHdwAddress path = makePubKeyAddressImpl (Just path)

makePubKeyAddressImpl :: Maybe HDAddressPayload -> PublicKey -> Address
makePubKeyAddressImpl path key = makeAddress spendingData attrs
 where
  spendingData = PubKeyASD key
  attrs        = AddrAttributes {aaPkDerivationPath = path}

-- | A function for making an address from a validation 'Script'
makeScriptAddress :: Script -> Address
makeScriptAddress scr = makeAddress spendingData attrs
 where
  spendingData = ScriptASD scr
  attrs        = AddrAttributes {aaPkDerivationPath = Nothing}

-- | A function for making an address from 'RedeemPublicKey'
makeRedeemAddress :: RedeemPublicKey -> Address
makeRedeemAddress key = makeAddress spendingData attrs
 where
  spendingData = RedeemASD key
  attrs        = AddrAttributes {aaPkDerivationPath = Nothing}

-- | Create address from secret key in hardened way
createHDAddressH
  :: ShouldCheckPassphrase
  -> PassPhrase
  -> HDPassphrase
  -> EncryptedSecretKey
  -> [Word32]
  -> Word32
  -> Maybe (Address, EncryptedSecretKey)
createHDAddressH scp passphrase hdPassphrase parent parentPath childIndex = do
  derivedSK <- deriveHDSecretKey scp passphrase parent childIndex
  let
    addressPayload =
      packHDAddressAttr hdPassphrase $ parentPath ++ [childIndex]
  let pk = encToPublic derivedSK
  return (makePubKeyHdwAddress addressPayload pk, derivedSK)

-- | Create address from public key via non-hardened way
createHDAddressNH
  :: HDPassphrase -> PublicKey -> [Word32] -> Word32 -> (Address, PublicKey)
createHDAddressNH passphrase parent parentPath childIndex = do
  let derivedPK = deriveHDPublicKey parent childIndex
  let
    addressPayload = packHDAddressAttr passphrase $ parentPath ++ [childIndex]
  (makePubKeyHdwAddress addressPayload derivedPK, derivedPK)


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

-- | Check if given 'Address' is created from given 'PublicKey'
checkPubKeyAddress :: PublicKey -> Address -> Bool
checkPubKeyAddress pk = checkAddrSpendingData (PubKeyASD pk)

-- | Check if given 'Address' is created from given validation script
checkScriptAddress :: Script -> Address -> Bool
checkScriptAddress script = checkAddrSpendingData (ScriptASD script)

-- | Check if given 'Address' is created from given 'RedeemPublicKey'
checkRedeemAddress :: RedeemPublicKey -> Address -> Bool
checkRedeemAddress rpk = checkAddrSpendingData (RedeemASD rpk)


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Get 'AddrAttributes' from 'Address'
addrAttributesUnwrapped :: Address -> AddrAttributes
addrAttributesUnwrapped = attrData . addrAttributes

-- | Makes account secret key for given wallet set
deriveLvl2KeyPair
  :: ShouldCheckPassphrase
  -> PassPhrase
  -> EncryptedSecretKey
  -- ^ key of wallet
  -> Word32
  -- ^ account derivation index
  -> Word32
  -- ^ address derivation index
  -> Maybe (Address, EncryptedSecretKey)
deriveLvl2KeyPair scp passphrase wsKey accountIndex addressIndex = do
  wKey <- deriveHDSecretKey scp passphrase wsKey accountIndex
  let hdPass = deriveHDPassphrase $ encToPublic wsKey
  -- We don't need to check passphrase twice
  createHDAddressH
    (ShouldCheckPassphrase False)
    passphrase
    hdPass
    wKey
    [accountIndex]
    addressIndex

deriveFirstHDAddress
  :: PassPhrase
  -> EncryptedSecretKey
  -- ^ key of wallet set
  -> Maybe (Address, EncryptedSecretKey)
deriveFirstHDAddress passphrase wsKey = deriveLvl2KeyPair
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

isUnknownAddressType :: Address -> Bool
isUnknownAddressType addr = case addrType addr of
  ATUnknown{} -> True
  _           -> False


--------------------------------------------------------------------------------
-- Maximal size
--------------------------------------------------------------------------------

-- | Largest (considering size of serialized data) PubKey address. Actual size
--   depends on CRC32 value which is serialized using var-length encoding.
largestPubKeyAddress :: Address
largestPubKeyAddress = makePubKeyAddress goodPk

-- | Maximal size of PubKey address.
maxPubKeyAddressSize :: Natural
maxPubKeyAddressSize = biSize largestPubKeyAddress

-- | Largest (considering size of serialized data) HD address with. Actual size
--   depends on CRC32 value which is serialized using var-length encoding.
largestHDAddress :: Address
largestHDAddress = case lvl2KeyPair of
  Nothing        -> panic "largestHDAddressBoot failed"
  Just (addr, _) -> addr
 where
  lvl2KeyPair = deriveLvl2KeyPair
    (ShouldCheckPassphrase False)
    emptyPassphrase
    encSK
    maxBound
    maxBound
  encSK = noPassEncrypt goodSk

-- | Maximal size of HD address
maxHDAddressSize :: Natural
maxHDAddressSize = biSize largestHDAddress

-- Public key and secret key for which we know that they produce
-- largest addresses in all cases we are interested in. It was checked
-- manually.
goodSkAndPk :: (PublicKey, SecretKey)
goodSkAndPk = deterministicKeyGen $ BS.replicate 32 0

goodPk :: PublicKey
goodPk = fst goodSkAndPk

goodSk :: SecretKey
goodSk = snd goodSkAndPk


-- Encodes the `Address` __without__ the CRC32.
-- It's important to keep this function separated from the `encode`
-- definition to avoid that `encode` would call `crc32` and
-- the latter invoke `crc32Update`, which would then try to call `encode`
-- indirectly once again, in an infinite loop.
encodeAddr :: Address -> Encoding
encodeAddr addr =
  encode (addrRoot addr) <> encode (addrAttributes addr) <> encode
    (addrType addr)

encodeAddrCRC32 :: Address -> Encoding
encodeAddrCRC32 addr =
  encodeCrcProtected (addrRoot addr, addrAttributes addr, addrType addr)
