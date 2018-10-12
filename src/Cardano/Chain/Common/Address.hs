{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Functionality related to 'Address' data type and related types.

module Cardano.Chain.Common.Address
       ( Address (..)
       , Address' (..)

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
       , isBootstrapEraDistrAddress

       -- * Construction
       , IsBootstrapEraAddr (..)
       , makeAddress
       , makePubKeyAddress
       , makePubKeyAddressBoot
       , makeRootPubKeyAddress
       , makePubKeyHdwAddress
       , makeScriptAddress
       , makeRedeemAddress

       , createHDAddressNH
       , createHDAddressH

       -- * Maximal sizes (needed for tx creation)
       , largestPubKeyAddressBoot
       , maxPubKeyAddressSizeBoot
       , largestPubKeyAddressSingleKey
       , maxPubKeyAddressSizeSingleKey
       , largestHDAddressBoot
       , maxHDAddressSizeBoot

       ) where

import           Cardano.Prelude

import           Control.Lens (makePrisms)
import           Control.Monad.Except (MonadError)
import qualified Data.Aeson as Aeson (FromJSON (..), FromJSONKey (..),
                     FromJSONKeyFunction (..), ToJSON (toJSON), ToJSONKey (..))
import qualified Data.Aeson.Types as Aeson (toJSONKeyText)
import qualified Data.ByteString as BS
import           Data.ByteString.Base58 (Alphabet (..), bitcoinAlphabet,
                     decodeBase58, encodeBase58)
import           Formatting (Format, bprint, build, builder, formatToString,
                     later, sformat, (%))
import qualified Formatting.Buildable as B
import           Text.JSON.Canonical (FromJSON (..), FromObjectKey (..),
                     JSValue (..), ToJSON (..), ToObjectKey (..))

import           Cardano.Binary.Class (Bi (..), DecoderError (..), Encoding,
                     biSize, encodeCrcProtected, encodedCrcProtectedSizeExpr)
import qualified Cardano.Binary.Class as Bi
import           Cardano.Chain.Common.AddrAttributes (AddrAttributes (..))
import           Cardano.Chain.Common.AddressHash (AddressHash, addressHash)
import           Cardano.Chain.Common.AddrSpendingData (AddrSpendingData (..),
                     AddrType (..), addrSpendingDataToType)
import           Cardano.Chain.Common.AddrStakeDistribution
                     (AddrStakeDistribution (..))
import           Cardano.Chain.Common.Attributes (Attributes (..), mkAttributes)
import           Cardano.Chain.Common.Script (Script)
import           Cardano.Chain.Common.StakeholderId (StakeholderId,
                     mkStakeholderId)
import           Cardano.Chain.Constants (accountGenesisIndex,
                     wAddressGenesisIndex)
import           Cardano.Crypto.Hashing (hashHexF)
import           Cardano.Crypto.HD (HDAddressPayload, HDPassphrase,
                     ShouldCheckPassphrase (..), deriveHDPassphrase,
                     deriveHDPublicKey, deriveHDSecretKey, packHDAddressAttr)
import           Cardano.Crypto.Signing (EncryptedSecretKey, PassPhrase,
                     PublicKey, RedeemPublicKey, SecretKey,
                     deterministicKeyGen, emptyPassphrase, encToPublic,
                     noPassEncrypt)


-- | Hash of this data is stored in 'Address'. This type exists mostly
-- for internal usage.
newtype Address' = Address'
    { unAddress' :: (AddrType, AddrSpendingData, Attributes AddrAttributes)
    } deriving (Eq, Show, Generic, Typeable)
      deriving newtype Bi

-- | 'Address' is where you can send coins.
data Address = Address
    { addrRoot       :: !(AddressHash Address')
    -- ^ Root of imaginary pseudo Merkle tree stored in this address.
    , addrAttributes :: !(Attributes AddrAttributes)
    -- ^ Attributes associated with this address.
    , addrType       :: !AddrType
    -- ^ The type of this address. Should correspond to
    -- 'AddrSpendingData', but it can't be checked statically, because
    -- spending data is hashed.
    } deriving (Eq, Ord, Generic, Typeable, Show)
      deriving anyclass NFData

instance Bi Address where
    encode addr = Bi.encodeCrcProtected
        (addrRoot addr, addrAttributes addr, addrType addr)

    decode = do
        (root, attributes, addrType') <- Bi.decodeCrcProtected
        pure $ Address
            { addrRoot = root
            , addrAttributes = attributes
            , addrType = addrType'
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

--------------------------------------------------------------------------------
-- Formatting, pretty-printing
--------------------------------------------------------------------------------

-- | A formatter showing guts of an 'Address'.
addressDetailedF :: Format r (Address -> r)
addressDetailedF = later $ \addr -> bprint
    (builder % " address with root " % hashHexF % ", attributes: " % build)
    (formattedType $ addrType addr)
    (addrRoot addr)
    (addrAttributes addr)
  where
    formattedType = \case
        ATPubKey      -> "PubKey"
        ATScript      -> "Script"
        ATRedeem      -> "Redeem"
        ATUnknown tag -> "Unknown#" <> B.build tag

-- | Currently we gonna use Bitcoin alphabet for representing addresses in
-- base58
addrAlphabet :: Alphabet
addrAlphabet = bitcoinAlphabet

addrToBase58 :: Address -> ByteString
addrToBase58 = encodeBase58 addrAlphabet . Bi.serialize'

instance B.Buildable Address where
    build = B.build . decodeUtf8 @Text . addrToBase58

-- | Specialized formatter for 'Address'.
addressF :: Format r (Address -> r)
addressF = build

-- | A function which decodes base58-encoded 'Address'
decodeTextAddress :: Text -> Either DecoderError Address
decodeTextAddress = decodeAddress . encodeUtf8
 where
  decodeAddress :: ByteString -> Either DecoderError Address
  decodeAddress bs = do
    let
      base58Err =
        DecoderErrorCustom "Address" "Invalid base58 representation of address"
    dbs <- maybeToRight base58Err $ decodeBase58 addrAlphabet bs
    Bi.decodeFull' dbs

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

-- | This newtype exists for clarity. It is used to tell pubkey
-- address creation functions whether an address is intended for
-- bootstrap era.
newtype IsBootstrapEraAddr = IsBootstrapEraAddr Bool

-- | A function for making an address from 'PublicKey'.
makePubKeyAddress :: IsBootstrapEraAddr -> PublicKey -> Address
makePubKeyAddress = makePubKeyAddressImpl Nothing

-- | A function for making an address from 'PublicKey' for bootstrap era.
makePubKeyAddressBoot :: PublicKey -> Address
makePubKeyAddressBoot = makePubKeyAddress (IsBootstrapEraAddr True)

-- | This function creates a root public key address. Stake
-- distribution doesn't matter for root addresses because by design
-- nobody should even use these addresses as outputs, so we can put
-- arbitrary distribution there. We use bootstrap era distribution
-- because its representation is more compact.
makeRootPubKeyAddress :: PublicKey -> Address
makeRootPubKeyAddress = makePubKeyAddressBoot

-- | A function for making an HDW address.
makePubKeyHdwAddress
    :: IsBootstrapEraAddr
    -> HDAddressPayload
    -- ^ Derivation path
    -> PublicKey
    -> Address
makePubKeyHdwAddress ibe path = makePubKeyAddressImpl (Just path) ibe

makePubKeyAddressImpl
    :: Maybe HDAddressPayload -> IsBootstrapEraAddr -> PublicKey -> Address
makePubKeyAddressImpl path (IsBootstrapEraAddr isBootstrapEra) key =
    makeAddress spendingData attrs
  where
    spendingData = PubKeyASD key
    distr
        | isBootstrapEra = BootstrapEraDistr
        | otherwise      = SingleKeyDistr (mkStakeholderId key)
    attrs = AddrAttributes
        { aaStakeDistribution = distr
        , aaPkDerivationPath  = path
        }

-- | A function for making an address from a validation 'Script'.  It
-- takes an optional 'StakeholderId'. If it's given, it will receive
-- the stake sent to the resulting 'Address'. Otherwise it's assumed
-- that an 'Address' is created for bootstrap era.
makeScriptAddress :: Maybe StakeholderId -> Script -> Address
makeScriptAddress stakeholder scr = makeAddress spendingData attrs
  where
    spendingData      = ScriptASD scr
    stakeDistribution = maybe BootstrapEraDistr SingleKeyDistr stakeholder
    attrs             = AddrAttributes
        { aaPkDerivationPath  = Nothing
        , aaStakeDistribution = stakeDistribution
        }

-- | A function for making an address from 'RedeemPublicKey'.
makeRedeemAddress :: RedeemPublicKey -> Address
makeRedeemAddress key = makeAddress spendingData attrs
  where
    spendingData = RedeemASD key
    attrs        = AddrAttributes
        { aaStakeDistribution = BootstrapEraDistr
        , aaPkDerivationPath  = Nothing
        }

-- | Create address from secret key in hardened way.
createHDAddressH
    :: IsBootstrapEraAddr
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> HDPassphrase
    -> EncryptedSecretKey
    -> [Word32]
    -> Word32
    -> Maybe (Address, EncryptedSecretKey)
createHDAddressH ibea scp passphrase hdPassphrase parent parentPath childIndex
    = do
        derivedSK <- deriveHDSecretKey scp passphrase parent childIndex
        let addressPayload =
                packHDAddressAttr hdPassphrase $ parentPath ++ [childIndex]
        let pk = encToPublic derivedSK
        return (makePubKeyHdwAddress ibea addressPayload pk, derivedSK)

-- | Create address from public key via non-hardened way.
createHDAddressNH
    :: IsBootstrapEraAddr
    -> HDPassphrase
    -> PublicKey
    -> [Word32]
    -> Word32
    -> (Address, PublicKey)
createHDAddressNH ibea passphrase parent parentPath childIndex = do
    let derivedPK = deriveHDPublicKey parent childIndex
    let addressPayload =
            packHDAddressAttr passphrase $ parentPath ++ [childIndex]
    (makePubKeyHdwAddress ibea addressPayload derivedPK, derivedPK)

--------------------------------------------------------------------------------
-- Checks
--------------------------------------------------------------------------------

-- | Check whether given 'AddrSpendingData' corresponds to given
-- 'Address'.
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

-- | Get 'AddrAttributes' from 'Address'.
addrAttributesUnwrapped :: Address -> AddrAttributes
addrAttributesUnwrapped = attrData . addrAttributes

-- | Makes account secret key for given wallet set.
deriveLvl2KeyPair
    :: IsBootstrapEraAddr
    -> ShouldCheckPassphrase
    -> PassPhrase
    -> EncryptedSecretKey
    -- ^ key of wallet
    -> Word32
    -- ^ account derivation index
    -> Word32
    -- ^ address derivation index
    -> Maybe (Address, EncryptedSecretKey)
deriveLvl2KeyPair ibea scp passphrase wsKey accountIndex addressIndex = do
    wKey <- deriveHDSecretKey scp passphrase wsKey accountIndex
    let hdPass = deriveHDPassphrase $ encToPublic wsKey
    -- We don't need to check passphrase twice
    createHDAddressH
        ibea
        (ShouldCheckPassphrase False)
        passphrase
        hdPass
        wKey
        [accountIndex]
        addressIndex

deriveFirstHDAddress
    :: IsBootstrapEraAddr
    -> PassPhrase
    -> EncryptedSecretKey
    -- ^ key of wallet set
    -> Maybe (Address, EncryptedSecretKey)
deriveFirstHDAddress ibea passphrase wsKey = deriveLvl2KeyPair
    ibea
    (ShouldCheckPassphrase False)
    passphrase
    wsKey
    accountGenesisIndex
    wAddressGenesisIndex

--------------------------------------------------------------------------------
-- Pattern-matching helpers
--------------------------------------------------------------------------------

-- | Check whether an 'Address' is redeem address.
isRedeemAddress :: Address -> Bool
isRedeemAddress addr = case addrType addr of
    ATRedeem -> True
    _        -> False

isUnknownAddressType :: Address -> Bool
isUnknownAddressType addr = case addrType addr of
    ATUnknown{} -> True
    _           -> False

-- | Check whether an 'Address' has bootstrap era stake distribution.
isBootstrapEraDistrAddress :: Address -> Bool
isBootstrapEraDistrAddress addr = case aaStakeDistribution aa of
    BootstrapEraDistr -> True
    _                 -> False
    where aa = addrAttributesUnwrapped addr

--------------------------------------------------------------------------------
-- Maximal size
--------------------------------------------------------------------------------

-- | Largest (considering size of serialized data) PubKey address with
-- BootstrapEra distribution. Actual size depends on CRC32 value which
-- is serialized using var-length encoding.
largestPubKeyAddressBoot :: Address
largestPubKeyAddressBoot = makePubKeyAddressBoot goodPk

-- | Maximal size of PubKey address with BootstrapEra
-- distribution (43).
maxPubKeyAddressSizeBoot :: Natural
maxPubKeyAddressSizeBoot = biSize largestPubKeyAddressBoot

-- | Largest (considering size of serialized data) PubKey address with
-- SingleKey distribution. Actual size depends on CRC32 value which
-- is serialized using var-length encoding.
largestPubKeyAddressSingleKey :: Address
largestPubKeyAddressSingleKey =
    makePubKeyAddress (IsBootstrapEraAddr False) goodPk

-- | Maximal size of PubKey address with SingleKey
-- distribution (78).
maxPubKeyAddressSizeSingleKey :: Natural
maxPubKeyAddressSizeSingleKey = biSize largestPubKeyAddressSingleKey

-- | Largest (considering size of serialized data) HD address with
-- BootstrapEra distribution. Actual size depends on CRC32 value which
-- is serialized using var-length encoding.
largestHDAddressBoot :: Address
largestHDAddressBoot = case lvl2KeyPair of
    Nothing        -> error "largestHDAddressBoot failed"
    Just (addr, _) -> addr
  where
    lvl2KeyPair = deriveLvl2KeyPair
        (IsBootstrapEraAddr True)
        (ShouldCheckPassphrase False)
        emptyPassphrase
        encSK
        maxBound
        maxBound
    encSK = noPassEncrypt goodSk

-- | Maximal size of HD address with BootstrapEra
-- distribution (76).
maxHDAddressSizeBoot :: Natural
maxHDAddressSizeBoot = biSize largestHDAddressBoot

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

makePrisms ''Address
