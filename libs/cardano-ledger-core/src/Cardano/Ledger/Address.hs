{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cardano.Ledger.Address (
  serialiseAddr,
  Addr (..),
  BootstrapAddress (..),
  bootstrapAddressAttrsSize,
  isBootstrapRedeemer,
  getNetwork,
  AccountAddress (..),
  AccountId (..),
  accountAddressAccountIdL,
  accountAddressCredentialL,
  accountAddressNetworkIdL,
  serialiseAccountAddress,
  deserialiseAccountAddress,
  putAccountAddress,
  decodeAccountAddress,
  fromCborAccountAddress,
  pattern RewardAccount,
  raNetwork,
  raCredential,
  rewardAccountCredentialL,
  rewardAccountNetworkL,
  serialiseRewardAccount,
  deserialiseRewardAccount,
  putRewardAccount,
  decodeRewardAccount,
  fromCborRewardAccount,
  bootstrapKeyHash,
  -- internals exported for testing
  putAddr,
  putCredential,
  putPtr,
  putVariableLengthWord64,
  Word7 (..),
  toWord7,

  -- * Compact Address
  fromBoostrapCompactAddress,
  compactAddr,
  decompactAddr,
  CompactAddr,
  unCompactAddr,
  isPayCredScriptCompactAddr,
  isBootstrapCompactAddr,
  decodeAddr,
  decodeAddrEither,
  decodeAddrStateT,
  decodeAddrStateLenientT,
  fromCborAddr,
  fromCborBothAddr,
  fromCborCompactAddr,
  fromCborRigorousBothAddr,
  fromCborBackwardsBothAddr,
  Withdrawals (..),
  DirectDeposits (..),
) where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.Hash.Class as Hash
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Ledger.BaseTypes (
  CertIx (..),
  Network (..),
  TxIx (..),
  byronProtVer,
  natVersion,
  networkToWord8,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  decodeFull',
  ifDecoderVersionAtLeast,
  serialize,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (
  Credential (..),
  Ptr (..),
  SlotNo32 (..),
  StakeReference (..),
  mkPtrNormalized,
 )
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Control.DeepSeq (NFData)
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Fail (FailT (..), runFail)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, get, modify', state)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Key as Aeson (fromText)
import qualified Data.Aeson.Types as Aeson
import Data.Binary (Put)
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import Data.Bits (Bits (clearBit, setBit, shiftL, shiftR, testBit, (.&.), (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short as SBS (ShortByteString, fromShort, index, length, toShort)
import Data.ByteString.Short.Internal as SBS (unsafeIndex)
import qualified Data.ByteString.Unsafe as BS (unsafeDrop, unsafeIndex, unsafeTake)
import Data.Default (Default (..))
import Data.Function (fix)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.MemPack (MemPack, Unpack (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import GHC.Show (intToDigit)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (NoThunks (..))
import Numeric (showIntAtBase)
import Quiet (Quiet (Quiet))

-- | Serialise an address to the external format.
serialiseAddr :: Addr -> ByteString
serialiseAddr = BSL.toStrict . B.runPut . putAddr
{-# INLINE serialiseAddr #-}

-- | Serialise an account address to the external format.
serialiseAccountAddress :: AccountAddress -> ByteString
serialiseAccountAddress = BSL.toStrict . B.runPut . putAccountAddress

-- | Deserialise an account address from the external format. This will fail if the
-- input data is not in the right format (or if there is trailing data).
deserialiseAccountAddress :: ByteString -> Maybe AccountAddress
deserialiseAccountAddress = decodeAccountAddress

{-# DEPRECATED serialiseRewardAccount "In favor of `serialiseAccountAddress`" #-}
serialiseRewardAccount :: AccountAddress -> ByteString
serialiseRewardAccount = serialiseAccountAddress

{-# DEPRECATED deserialiseRewardAccount "In favor of `deserialiseAccountAddress`" #-}
deserialiseRewardAccount :: ByteString -> Maybe AccountAddress
deserialiseRewardAccount = deserialiseAccountAddress

-- | An address for UTxO.
--
-- Contents of Addr data type are intentionally left as lazy, otherwise
-- operating on compact form of an address will result in redundant work.
data Addr
  = Addr Network (Credential Payment) StakeReference
  | AddrBootstrap BootstrapAddress
  deriving (Show, Eq, Generic, NFData, Ord)

-- | Lookup a Network Id for an Address
getNetwork :: Addr -> Network
getNetwork (Addr n _ _) = n
getNetwork (AddrBootstrap (BootstrapAddress byronAddr)) =
  case Byron.aaNetworkMagic . Byron.attrData . Byron.addrAttributes $ byronAddr of
    Byron.NetworkMainOrStage -> Mainnet
    Byron.NetworkTestnet _ -> Testnet

instance NoThunks Addr

-- | An account based address for rewards
data AccountAddress = AccountAddress
  { aaNetworkId :: !Network
  , aaAccountId :: !AccountId
  }
  deriving (Show, Eq, Generic, Ord, NFData, ToJSONKey, FromJSONKey)

newtype AccountId = AccountId {unAccountId :: Credential Staking}
  deriving (Show, Eq, Generic, Ord)
  deriving newtype (NFData, NoThunks, ToJSON, FromJSON, EncCBOR, DecCBOR)

-- | Deprecated pattern synonym for backward compatibility
pattern RewardAccount :: Network -> Credential Staking -> AccountAddress
pattern RewardAccount {raNetwork, raCredential} = AccountAddress raNetwork (AccountId raCredential)
{-# DEPRECATED RewardAccount "In favor of `AccountAddress`" #-}

{-# DEPRECATED raNetwork "In favor of `aaNetworkId`" #-}

{-# DEPRECATED raCredential "In favor of `aaAccountId`" #-}

{-# COMPLETE RewardAccount #-}

accountAddressAccountIdL :: Lens' AccountAddress AccountId
accountAddressAccountIdL = lens aaAccountId $ \x y -> x {aaAccountId = y}

accountAddressCredentialL :: Lens' AccountAddress (Credential Staking)
accountAddressCredentialL = lens (\(AccountAddress _ (AccountId c)) -> c) $ \x y -> x {aaAccountId = AccountId y}

accountAddressNetworkIdL :: Lens' AccountAddress Network
accountAddressNetworkIdL = lens aaNetworkId $ \x y -> x {aaNetworkId = y}

{-# DEPRECATED rewardAccountCredentialL "In favor of `accountAddressCredentialL`" #-}
rewardAccountCredentialL :: Lens' AccountAddress (Credential Staking)
rewardAccountCredentialL = accountAddressCredentialL

{-# DEPRECATED rewardAccountNetworkL "In favor of `accountAddressNetworkIdL`" #-}
rewardAccountNetworkL :: Lens' AccountAddress Network
rewardAccountNetworkL = accountAddressNetworkIdL

instance Default AccountAddress where
  def = AccountAddress def (AccountId def)

instance ToJSON AccountAddress where
  toJSON aa =
    Aeson.object
      [ "network" .= aaNetworkId aa
      , "credential" .= (aa ^. accountAddressCredentialL)
      ]

instance FromJSON AccountAddress where
  parseJSON =
    Aeson.withObject "AccountAddress" $ \obj ->
      AccountAddress
        <$> obj
          .: "network"
        <*> (AccountId <$> obj .: "credential")

instance NoThunks AccountAddress

instance ToJSONKey Addr where
  toJSONKey = Aeson.ToJSONKeyText (Aeson.fromText . addrToText) (Aeson.text . addrToText)

instance FromJSONKey Addr where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseAddr

instance ToJSON Addr where
  toJSON = toJSON . addrToText

instance FromJSON Addr where
  parseJSON = Aeson.withText "address" parseAddr

addrToText :: Addr -> Text
addrToText = Text.decodeLatin1 . B16.encode . serialiseAddr

parseAddr :: Text -> Aeson.Parser Addr
parseAddr t = do
  bytes <- either badHex return (B16.decode (Text.encodeUtf8 t))
  decodeAddr bytes
  where
    badHex h = fail $ "Addresses are expected in hex encoding for now: " ++ show h

byron :: Int
byron = 7

notBaseAddr :: Int
notBaseAddr = 6

isEnterpriseAddr :: Int
isEnterpriseAddr = 5

stakeCredIsScript :: Int
stakeCredIsScript = 5

payCredIsScript :: Int
payCredIsScript = 4

putAddr :: Addr -> Put
putAddr (AddrBootstrap (BootstrapAddress byronAddr)) =
  B.putLazyByteString (serialize byronProtVer byronAddr)
putAddr (Addr network pc sr) =
  let setPayCredBit = case pc of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
   in case sr of
        StakeRefBase sc -> do
          let setStakeCredBit = case sc of
                ScriptHashObj _ -> flip setBit stakeCredIsScript
                KeyHashObj _ -> id
              header = setStakeCredBit . setPayCredBit $ netId
          B.putWord8 header
          putCredential pc
          putCredential sc
        StakeRefPtr ptr -> do
          let header = setPayCredBit $ netId `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc
          putPtr ptr
        StakeRefNull -> do
          let header = setPayCredBit $ netId `setBit` isEnterpriseAddr `setBit` notBaseAddr
          B.putWord8 header
          putCredential pc
{-# INLINE putAddr #-}

putAccountAddress :: AccountAddress -> Put
putAccountAddress (AccountAddress network (AccountId cred)) = do
  let setPayCredBit = case cred of
        ScriptHashObj _ -> flip setBit payCredIsScript
        KeyHashObj _ -> id
      netId = networkToWord8 network
      accountAddressPrefix = 0xE0 -- 0b11100000 are always set for account addresses
      header = setPayCredBit (netId .|. accountAddressPrefix)
  B.putWord8 header
  putCredential cred
{-# INLINE putAccountAddress #-}

{-# DEPRECATED putRewardAccount "In favor of `putAccountAddress`" #-}
putRewardAccount :: AccountAddress -> Put
putRewardAccount = putAccountAddress
{-# INLINE putRewardAccount #-}

putHash :: Hash.Hash h a -> Put
putHash = B.putByteString . Hash.hashToBytes
{-# INLINE putHash #-}

putCredential :: Credential kr -> Put
putCredential (ScriptHashObj (ScriptHash h)) = putHash h
putCredential (KeyHashObj (KeyHash h)) = putHash h
{-# INLINE putCredential #-}

-- | The size of the extra attributes in a bootstrap (ie Byron) address. Used
-- to help enforce that people do not post huge ones on the chain.
bootstrapAddressAttrsSize :: BootstrapAddress -> Int
bootstrapAddressAttrsSize (BootstrapAddress addr) =
  maybe 0 payloadLen derivationPath + Byron.unknownAttributesLength attrs
  where
    payloadLen = BS.length . Byron.getHDAddressPayload
    derivationPath = Byron.aaVKDerivationPath (Byron.attrData attrs)
    attrs = Byron.addrAttributes addr

-- | Return True if a given address is a redeemer address from the Byron Era
isBootstrapRedeemer :: BootstrapAddress -> Bool
isBootstrapRedeemer (BootstrapAddress (Byron.Address _ _ Byron.ATRedeem)) = True
isBootstrapRedeemer _ = False

putPtr :: Ptr -> Put
putPtr (Ptr (SlotNo32 slot) (TxIx txIx) (CertIx certIx)) = do
  putVariableLengthWord64 (fromIntegral slot)
  putVariableLengthWord64 (fromIntegral txIx) -- TODO: switch to using MemPack for compacting Address at which point
  putVariableLengthWord64 (fromIntegral certIx) --     this conversion from Word16 to Word64 will no longer be necessary

newtype Word7 = Word7 Word8
  deriving (Eq, Show)

toWord7 :: Word8 -> Word7
toWord7 x = Word7 (x .&. 0x7F) -- 0x7F = 0b01111111

putWord7s :: [Word7] -> Put
putWord7s [] = pure ()
putWord7s [Word7 x] = B.putWord8 x
putWord7s (Word7 x : xs) = B.putWord8 (x .|. 0x80) >> putWord7s xs

word64ToWord7s :: Word64 -> [Word7]
word64ToWord7s = reverse . go
  where
    go :: Word64 -> [Word7]
    go n
      | n > 0x7F = (toWord7 . fromIntegral) n : go (shiftR n 7)
      | otherwise = [Word7 . fromIntegral $ n]

putVariableLengthWord64 :: Word64 -> Put
putVariableLengthWord64 = putWord7s . word64ToWord7s

instance EncCBOR Addr where
  encCBOR = encCBOR . B.runPut . putAddr
  {-# INLINE encCBOR #-}

instance DecCBOR Addr where
  decCBOR = fromCborAddr
  {-# INLINE decCBOR #-}

instance EncCBOR AccountAddress where
  encCBOR = encCBOR . B.runPut . putAccountAddress
  {-# INLINE encCBOR #-}

instance DecCBOR AccountAddress where
  decCBOR = fromCborAccountAddress
  {-# INLINE decCBOR #-}

newtype BootstrapAddress = BootstrapAddress
  { unBootstrapAddress :: Byron.Address
  }
  deriving (Eq, Generic)
  deriving newtype (NFData, Ord)
  deriving (Show) via Quiet BootstrapAddress

instance NoThunks BootstrapAddress

bootstrapKeyHash ::
  BootstrapAddress ->
  KeyHash Payment
bootstrapKeyHash (BootstrapAddress byronAddress) =
  let root = Byron.addrRoot byronAddress
      bytes = Byron.abstractHashToBytes root
      !hash =
        fromMaybe (error "bootstrapKeyHash: incorrect hash length") $
          Hash.hashFromBytes bytes
   in KeyHash hash

------------------------------------------------------------------------------------------
-- Compact Address -----------------------------------------------------------------------
------------------------------------------------------------------------------------------

newtype CompactAddr = UnsafeCompactAddr ShortByteString
  deriving stock (Eq, Generic, Ord)
  deriving newtype (NoThunks, NFData, MemPack)

instance Show CompactAddr where
  show c = show (decompactAddr c)

-- | Unwrap the compact address and get to the address' binary representation.
unCompactAddr :: CompactAddr -> ShortByteString
unCompactAddr (UnsafeCompactAddr sbs) = sbs
{-# INLINE unCompactAddr #-}

compactAddr :: Addr -> CompactAddr
compactAddr = UnsafeCompactAddr . SBS.toShort . serialiseAddr
{-# INLINE compactAddr #-}

decompactAddr :: HasCallStack => CompactAddr -> Addr
decompactAddr (UnsafeCompactAddr sbs) =
  case runFail $ evalStateT (decodeAddrStateLenientT True True sbs) 0 of
    Right addr -> addr
    Left err ->
      error $
        "Impossible: Malformed CompactAddr was allowed into the system. "
          ++ " Decoder error: "
          ++ err
{-# INLINE decompactAddr #-}

------------------------------------------------------------------------------------------
-- Address Serializer --------------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | Decoder for an `Addr`. Works in all eras
fromCborAddr :: Decoder s Addr
fromCborAddr = fst <$> fromCborBothAddr
{-# INLINE fromCborAddr #-}

-- | Returns the actual bytes that represent an addres, while ensuring that they can
-- be decoded in any era as an `Addr` when need be.
fromCborCompactAddr :: Decoder s CompactAddr
fromCborCompactAddr = snd <$> fromCborBothAddr
{-# INLINE fromCborCompactAddr #-}

-- | This is the decoder for an address that returns both the actual `Addr` and the bytes,
-- that it was encoded as.
fromCborBothAddr :: Decoder s (Addr, CompactAddr)
fromCborBothAddr = do
  ifDecoderVersionAtLeast
    (natVersion @7)
    ( ifDecoderVersionAtLeast
        (natVersion @9)
        (fromCborRigorousBothAddr False)
        (fromCborRigorousBothAddr True)
    )
    fromCborBackwardsBothAddr
{-# INLINE fromCborBothAddr #-}

-- | Starting with Babbage we no longer allow addresses with garbage in them.
fromCborRigorousBothAddr ::
  -- | Should there be a hard failure for garbage pointers (`False`) or should they be normalized instead (`True`)
  Bool ->
  Decoder s (Addr, CompactAddr)
fromCborRigorousBothAddr isPtrLenient = do
  sbs <- decCBOR
  flip evalStateT 0 $ do
    addr <- decodeAddrStateLenientT isPtrLenient False sbs
    pure (addr, UnsafeCompactAddr sbs)
{-# INLINE fromCborRigorousBothAddr #-}

-- | Prior to Babbage era we did not check if a binary blob representing an address was
-- fully consumed, so unfortunately we must preserve this behavior. However, we do not
-- need to preserve the unconsumed bytes in memory, therefore we can drop the
-- garbage after we successfully decoded the malformed address. We also need to allow
-- bogus pointer address to be deserializeable prior to Babbage era.
fromCborBackwardsBothAddr :: Decoder s (Addr, CompactAddr)
fromCborBackwardsBothAddr = do
  sbs <- decCBOR
  flip evalStateT 0 $ do
    addr <- decodeAddrStateLenientT True True sbs
    bytesConsumed <- get
    let sbsCropped = SBS.toShort $ BS.take bytesConsumed $ SBS.fromShort sbs
    pure (addr, UnsafeCompactAddr sbsCropped)
{-# INLINE fromCborBackwardsBothAddr #-}

class AddressBuffer b where
  bufLength :: b -> Int

  bufUnsafeIndex :: b -> Int -> Word8

  bufToByteString :: b -> BS.ByteString

  bufGetHash :: Hash.HashAlgorithm h => b -> Int -> Maybe (Hash.Hash h a)

instance AddressBuffer ShortByteString where
  bufLength = SBS.length
  {-# INLINE bufLength #-}
  bufUnsafeIndex = SBS.unsafeIndex
  {-# INLINE bufUnsafeIndex #-}
  bufToByteString = SBS.fromShort
  {-# INLINE bufToByteString #-}
  bufGetHash = Hash.hashFromOffsetBytesShort
  {-# INLINE bufGetHash #-}

instance AddressBuffer BS.ByteString where
  bufLength = BS.length
  {-# INLINE bufLength #-}
  bufUnsafeIndex = BS.unsafeIndex
  {-# INLINE bufUnsafeIndex #-}
  bufToByteString = id
  {-# INLINE bufToByteString #-}
  bufGetHash :: forall h a. Hash.HashAlgorithm h => BS.ByteString -> Int -> Maybe (Hash.Hash h a)
  bufGetHash bs offset = do
    let size = fromIntegral (Hash.sizeHash (Proxy :: Proxy h))
    guard (offset >= 0 && offset + size <= BS.length bs)
    Hash.hashFromBytes (BS.unsafeTake size (BS.unsafeDrop offset bs))
  {-# INLINE bufGetHash #-}

-- | Address header byte truth table:
newtype Header = Header Word8
  deriving newtype (Eq, Ord, Bits, Num)

instance Show Header where
  show (Header header) = ("0b" ++) . showIntAtBase 2 intToDigit header $ ""

-- | Every Byron address starts with @[TkListLen 2]@, which encodes as 130 (or 0x80)
headerByron :: Header
headerByron = 0b10000010 -- 0x80

isByronAddress :: Header -> Bool
isByronAddress = (== headerByron)
{-# INLINE isByronAddress #-}

headerNonShelleyBits :: Header
headerNonShelleyBits = headerByron .|. 0b00001100

headerNetworkId :: Header -> Network
headerNetworkId header
  | header `testBit` 0 = Mainnet
  | otherwise = Testnet
{-# INLINE headerNetworkId #-}

headerIsPaymentScript :: Header -> Bool
headerIsPaymentScript = (`testBit` 4)
{-# INLINE headerIsPaymentScript #-}

headerIsEnterpriseAddr :: Header -> Bool
headerIsEnterpriseAddr = (`testBit` 5)
{-# INLINE headerIsEnterpriseAddr #-}

headerIsStakingScript :: Header -> Bool
headerIsStakingScript = (`testBit` 5)
{-# INLINE headerIsStakingScript #-}

headerIsBaseAddress :: Header -> Bool
headerIsBaseAddress = not . (`testBit` 6)
{-# INLINE headerIsBaseAddress #-}

-- | Same as `decodeAddr`, but produces an `Either` result
decodeAddrEither ::
  BS.ByteString ->
  Either String Addr
decodeAddrEither bs = runFail $ evalStateT (decodeAddrStateT bs) 0
{-# INLINE decodeAddrEither #-}

-- | Strict decoder for an address from a `ByteString`. This will not let you decode some
-- of the buggy addresses that have been placed on chain. This decoder is intended for
-- addresses that are to be placed on chian today.
decodeAddr ::
  forall m.
  MonadFail m =>
  BS.ByteString ->
  m Addr
decodeAddr bs = evalStateT (decodeAddrStateT bs) 0
{-# INLINE decodeAddr #-}

-- | Just like `decodeAddrStateLenientT`, but enforces the address to be well-formed.
decodeAddrStateT ::
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m Addr
decodeAddrStateT = decodeAddrStateLenientT False False
{-# INLINE decodeAddrStateT #-}

-- | This is the most general decoder for a Cardano address. This function is not meant to
-- be used directly, but it is exported for convenice. `decodeAddr` and other should be
-- used instead.
--
-- While decoding an Addr the header (the first byte in the buffer) is expected to be in a
-- certain format. Here are the meaning of all the bits:
--
-- @@@
--
-- ┏━━━━━━━━━━━━━━━━┳━┯━┯━┯━┯━┯━┯━┯━┓
-- ┃  Byron Address ┃1┊0┊0┊0┊0┊0┊1┊0┃
-- ┣━━━━━━━━━━━━━━━━╋━┿━┿━┿━┿━┿━┿━┿━┫
-- ┃Shelley Address ┃0┊x┊x┊x┊0┊0┊0┊x┃
-- ┗━━━━━━━━━━━━━━━━╋━┿━┿━┿━┿━┿━┿━┿━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
--                  ┃0┊0┊0┊0┊0┊0┊0┊0┃ Testnet PaymentKey    StakingKey    ┃
--                  ┃0┊0┊0┊0┊0┊0┊0┊1┃ Mainnet PaymentKey    StakingKey    ┃
--                  ┃0┊0┊0┊1┊0┊0┊0┊0┃ Testnet PaymentScript StakingKey    ┃
--                  ┃0┊0┊0┊1┊0┊0┊0┊1┃ Mainnet PaymentScript StakingKey    ┃
--                  ┃0┊0┊1┊0┊0┊0┊0┊0┃ Testnet PaymentKey    StakingScript ┃
--                  ┃0┊0┊1┊0┊0┊0┊0┊1┃ Mainnet PaymentKey    StakingScript ┃
--                  ┃0┊0┊1┊1┊0┊0┊0┊0┃ Testnet PaymentScript StakingScript ┃
--                  ┃0┊0┊1┊1┊0┊0┊0┊1┃ Mainnet PaymentScript StakingScript ┃
--                  ┃0┊1┊0┊0┊0┊0┊0┊0┃ Testnet PaymentKey    StakingPtr    ┃
--                  ┃0┊1┊0┊0┊0┊0┊0┊1┃ Mainnet PaymentKey    StakingPtr    ┃
--                  ┃0┊1┊0┊1┊0┊0┊0┊0┃ Testnet PaymentScript StakingPtr    ┃
--                  ┃0┊1┊0┊1┊0┊0┊0┊1┃ Mainnet PaymentScript StakingPtr    ┃
--                  ┃0┊1┊1┊0┊0┊0┊0┊0┃ Testnet PaymentKey    StakingNull   ┃
--                  ┃0┊1┊1┊0┊0┊0┊0┊1┃ Mainnet PaymentKey    StakingNull   ┃
--                  ┃0┊1┊1┊1┊0┊0┊0┊0┃ Testnet PaymentScript StakingNull   ┃
--                  ┃0┊1┊1┊1┊0┊0┊0┊1┃ Mainnet PaymentScript StakingNull   ┃
--                  ┗━┷━┷━┷━┷━┷━┷━┷━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
--                      \ \ \       \
--                       \ \ \       `Is Mainnet Address
--                        \ \ `Payment Credential is a Script
--                         \ `Staking Credential is a Script / No Staking Credential
--                          `Not a Base Address
-- @@@
decodeAddrStateLenientT ::
  (MonadFail m, AddressBuffer b) =>
  -- | Enable lenient decoding for Ptrs, i.e. indicate whether junk can follow a Ptr. This
  -- is necessary for backwards compatibility only.  Setting this argument to True is only
  -- needed for backwards compatibility.
  Bool ->
  -- | Indicate whether decoder should not enforce the full input to be consumed or not,
  -- i.e. allow garbage at the end or not. Setting this argument to True is only needed
  -- for backwards compatibility.
  Bool ->
  b ->
  StateT Int m Addr
decodeAddrStateLenientT isPtrLenient isLenient buf = do
  guardLength "Header" 1 buf
  let header = Header $ bufUnsafeIndex buf 0
  addr <-
    if isByronAddress header
      then AddrBootstrap <$> decodeBootstrapAddress buf
      else do
        -- Ensure there are no unexpected bytes in the header
        unless (header .&. headerNonShelleyBits == 0)
          $ failDecoding
            "Shelley Address"
          $ "Invalid header. Unused bits are not suppose to be set: " <> show header
        -- Advance one byte for the consumed header
        modify' (+ 1)
        payment <- decodePaymentCredential header buf
        staking <- decodeStakeReference isPtrLenient header buf
        pure $ Addr (headerNetworkId header) payment staking
  unless isLenient $
    ensureBufIsConsumed "Addr" buf
  pure addr
{-# INLINE decodeAddrStateLenientT #-}

-- | Checks that the current offset is exactly at the end of the buffer.
ensureBufIsConsumed ::
  forall m b.
  (MonadFail m, AddressBuffer b) =>
  -- | Name for error reporting
  String ->
  -- | Buffer that should have been consumed.
  b ->
  StateT Int m ()
ensureBufIsConsumed name buf = do
  lastOffset <- get
  let len = bufLength buf
  unless (lastOffset == len) $
    failDecoding name $
      "Left over bytes: " ++ show (len - lastOffset)
{-# INLINE ensureBufIsConsumed #-}

-- | This decoder assumes the whole `ShortByteString` is occupied by the `BootstrapAddress`
decodeBootstrapAddress ::
  forall m b.
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m BootstrapAddress
decodeBootstrapAddress buf =
  case decodeFull' byronProtVer $ bufToByteString buf of
    Left e -> fail $ show e
    Right addr -> BootstrapAddress addr <$ modify' (+ bufLength buf)
{-# INLINE decodeBootstrapAddress #-}

decodePaymentCredential ::
  (MonadFail m, AddressBuffer b) =>
  Header ->
  b ->
  StateT Int m (Credential Payment)
decodePaymentCredential header buf
  | headerIsPaymentScript header = ScriptHashObj <$> decodeScriptHash buf
  | otherwise = KeyHashObj <$> decodeKeyHash buf
{-# INLINE decodePaymentCredential #-}

decodeStakeReference ::
  (MonadFail m, AddressBuffer b) =>
  Bool ->
  Header ->
  b ->
  StateT Int m StakeReference
decodeStakeReference isLenientPtrDecoder header buf
  | headerIsBaseAddress header =
      if headerIsStakingScript header
        then StakeRefBase . ScriptHashObj <$> decodeScriptHash buf
        else StakeRefBase . KeyHashObj <$> decodeKeyHash buf
  | otherwise =
      if headerIsEnterpriseAddr header
        then pure StakeRefNull
        else StakeRefPtr <$> if isLenientPtrDecoder then decodePtrLenient buf else decodePtr buf
{-# INLINE decodeStakeReference #-}

decodeKeyHash ::
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (KeyHash kr)
decodeKeyHash buf = KeyHash <$> decodeHash buf
{-# INLINE decodeKeyHash #-}

decodeScriptHash ::
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m ScriptHash
decodeScriptHash buf = ScriptHash <$> decodeHash buf
{-# INLINE decodeScriptHash #-}

decodeHash ::
  forall a h m b.
  (Hash.HashAlgorithm h, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (Hash.Hash h a)
decodeHash buf = do
  offset <- get
  case bufGetHash buf offset of
    Just h -> h <$ modify' (+ hashLen)
    Nothing
      | offset >= 0 ->
          failDecoding "Hash" $
            "Not enough bytes supplied: "
              ++ show (bufLength buf - offset)
              ++ ". Expected: "
              ++ show hashLen
    Nothing -> fail "Impossible: Negative offset"
  where
    hashLen :: Int
    hashLen = fromIntegral (Hash.sizeHash (Proxy :: Proxy h))
{-# INLINE decodeHash #-}

decodePtr ::
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m Ptr
decodePtr buf =
  Ptr
    <$> (SlotNo32 <$> decodeVariableLengthWord32 "SlotNo" buf)
    <*> (TxIx <$> decodeVariableLengthWord16 "TxIx" buf)
    <*> (CertIx <$> decodeVariableLengthWord16 "CertIx" buf)
{-# INLINE decodePtr #-}

decodePtrLenient ::
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m Ptr
decodePtrLenient buf =
  mkPtrNormalized
    <$> decodeVariableLengthWord64 "SlotNo" buf
    <*> decodeVariableLengthWord64 "TxIx" buf
    <*> decodeVariableLengthWord64 "CertIx" buf
{-# INLINE decodePtrLenient #-}

guardLength ::
  (MonadFail m, AddressBuffer b) =>
  -- | Name for what is being decoded for the error message
  String ->
  Int ->
  b ->
  StateT Int m ()
guardLength name expectedLength buf = do
  offset <- get
  when (offset > bufLength buf - expectedLength) $
    failDecoding name "Not enough bytes for decoding"
{-# INLINE guardLength #-}

-- | Decode a variable length integral value that is encoded with 7 bits of data
-- and the most significant bit (MSB), the 8th bit is set whenever there are
-- more bits following. Continuation style allows us to avoid
-- rucursion. Removing loops is good for performance.
decode7BitVarLength ::
  (Num a, Bits a, AddressBuffer b, MonadFail m) =>
  -- | Name of what is being decoded for error reporting
  String ->
  -- | Buffer that contains encoded number
  b ->
  -- | Continuation that will be invoked if MSB is set
  (a -> StateT Int m a) ->
  -- | Accumulator
  a ->
  StateT Int m a
decode7BitVarLength name buf cont !acc = do
  guardLength name 1 buf
  offset <- state (\off -> (off, off + 1))
  let b8 = bufUnsafeIndex buf offset
  if b8 `testBit` 7
    then cont (acc `shiftL` 7 .|. fromIntegral (b8 `clearBit` 7))
    else pure (acc `shiftL` 7 .|. fromIntegral b8)
{-# INLINE decode7BitVarLength #-}

failDecoding :: MonadFail m => String -> String -> m a
failDecoding name msg = fail $ "Decoding " ++ name ++ ": " ++ msg
{-# NOINLINE failDecoding #-}

decodeVariableLengthWord16 ::
  forall m b.
  (MonadFail m, AddressBuffer b) =>
  String ->
  b ->
  StateT Int m Word16
decodeVariableLengthWord16 name buf = do
  off0 <- get
  let d7 = decode7BitVarLength name buf
      d7last :: Word16 -> StateT Int m Word16
      d7last acc = do
        res <- decode7BitVarLength name buf (\_ -> failDecoding name "too many bytes.") acc
        -- Only while decoding the last 7bits we check if there was too many
        -- bits supplied at the beginning.
        unless (bufUnsafeIndex buf off0 .&. 0b11111100 == 0b10000000) $
          failDecoding name "More than 16bits was supplied"
        pure res
  d7 (d7 d7last) 0
{-# INLINE decodeVariableLengthWord16 #-}

decodeVariableLengthWord32 ::
  forall m b.
  (MonadFail m, AddressBuffer b) =>
  String ->
  b ->
  StateT Int m Word32
decodeVariableLengthWord32 name buf = do
  off0 <- get
  let d7 = decode7BitVarLength name buf
      {-# INLINE d7 #-}
      d7last :: Word32 -> StateT Int m Word32
      d7last acc = do
        res <- decode7BitVarLength name buf (\_ -> failDecoding name "too many bytes.") acc
        -- Only while decoding the last 7bits we check if there was too many
        -- bits supplied at the beginning.
        unless (bufUnsafeIndex buf off0 .&. 0b11110000 == 0b10000000) $
          failDecoding name "More than 32bits was supplied"
        pure res
      {-# INLINE d7last #-}
  d7 (d7 (d7 (d7 d7last))) 0
{-# INLINE decodeVariableLengthWord32 #-}

-- | This decoder is here only with the purpose of preserving old buggy behavior. Should
-- not be used for anything else.
decodeVariableLengthWord64 ::
  forall m b.
  (MonadFail m, AddressBuffer b) =>
  String ->
  b ->
  StateT Int m Word64
decodeVariableLengthWord64 name buf = fix (decode7BitVarLength name buf) 0
{-# INLINE decodeVariableLengthWord64 #-}

------------------------------------------------------------------------------------------
-- Account Address Deserializer ----------------------------------------------------------
------------------------------------------------------------------------------------------

decodeAccountAddress ::
  forall b m.
  (AddressBuffer b, MonadFail m) =>
  b ->
  m AccountAddress
decodeAccountAddress buf = evalStateT (decodeAccountAddressT buf) 0

fromCborAccountAddress :: Decoder s AccountAddress
fromCborAccountAddress = do
  sbs :: ShortByteString <- decCBOR
  decodeAccountAddress sbs

{-# DEPRECATED decodeRewardAccount "In favor of `decodeAccountAddress`" #-}
decodeRewardAccount ::
  forall b m.
  (AddressBuffer b, MonadFail m) =>
  b ->
  m AccountAddress
decodeRewardAccount = decodeAccountAddress

{-# DEPRECATED fromCborRewardAccount "In favor of `fromCborAccountAddress`" #-}
fromCborRewardAccount :: Decoder s AccountAddress
fromCborRewardAccount = fromCborAccountAddress

headerIsAccountAddress :: Header -> Bool
headerIsAccountAddress header = header .&. 0b11101110 == 0b11100000
{-# INLINE headerIsAccountAddress #-}

headerAccountAddressIsScript :: Header -> Bool
headerAccountAddressIsScript = (`testBit` 4)
{-# INLINE headerAccountAddressIsScript #-}

-- | Account Address Header.
--
-- @@@
--
-- ┏━━━━━━━━━━━━━━━━━━┳━┯━┯━┯━┯━┯━┯━┯━┓
-- ┃ Account Address  ┃1┊1┊1┊x┊0┊0┊0┊x┃
-- ┗━━━━━━━━━━━━━━━━━━╋━┿━┿━┿━┿━┿━┿━┿━╋━━━━━━━━━━━━━━━━━━━━━━━┓
--                    ┃1┊1┊1┊0┊0┊0┊0┊0┃ Testnet StakingKey    ┃
--                    ┃1┊1┊1┊0┊0┊0┊0┊1┃ Mainnet StakingKey    ┃
--                    ┃1┊1┊1┊1┊0┊0┊0┊0┃ Testnet StakingScript ┃
--                    ┃1┊1┊1┊1┊0┊0┊0┊1┃ Mainnet StakingScript ┃
--                    ┗━┷━┷━┷━┷━┷━┷━┷━┻━━━━━━━━━━━━━━━━━━━━━━━┛
--                            \       \
--                             \       `Is Mainnet Address
--                              `Account Credential is a Script
-- @@@
decodeAccountAddressT ::
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m AccountAddress
decodeAccountAddressT buf = do
  guardLength "Header" 1 buf
  modify' (+ 1)
  let header = Header $ bufUnsafeIndex buf 0
  unless (headerIsAccountAddress header) $
    fail $
      "Invalid header for the account address: " <> show header
  account <-
    if headerAccountAddressIsScript header
      then ScriptHashObj <$> decodeScriptHash buf
      else KeyHashObj <$> decodeKeyHash buf
  ensureBufIsConsumed "AccountAddress" buf
  pure $! AccountAddress (headerNetworkId header) (AccountId account)
{-# INLINE decodeAccountAddressT #-}

instance EncCBOR CompactAddr where
  encCBOR (UnsafeCompactAddr bytes) = encCBOR bytes
  {-# INLINE encCBOR #-}

instance DecCBOR CompactAddr where
  decCBOR = fromCborCompactAddr
  {-# INLINE decCBOR #-}

-- | Efficiently check whether compacted adddress is an address with a credential
-- that is a payment script.
isPayCredScriptCompactAddr :: CompactAddr -> Bool
isPayCredScriptCompactAddr (UnsafeCompactAddr bytes) =
  testBit (SBS.index bytes 0) payCredIsScript

-- | Efficiently check whether compated adddress is a Byron address.
isBootstrapCompactAddr :: CompactAddr -> Bool
isBootstrapCompactAddr (UnsafeCompactAddr bytes) = testBit (SBS.index bytes 0) byron

-- | Convert Byron's comapct address into `CompactAddr`. This is just an efficient type cast.
fromBoostrapCompactAddress :: Byron.CompactAddress -> CompactAddr
fromBoostrapCompactAddress = UnsafeCompactAddr . Byron.unsafeGetCompactAddress

-- | This is called @wdrl@ in the spec.
newtype Withdrawals = Withdrawals {unWithdrawals :: Map AccountAddress Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData, EncCBOR, DecCBOR)

-- | Direct deposits to account addresses.
newtype DirectDeposits = DirectDeposits {unDirectDeposits :: Map AccountAddress Coin}
  deriving (Show, Eq, Generic)
  deriving newtype (NoThunks, NFData, EncCBOR, DecCBOR)
