{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.CompactAddress
  ( Addr (..),
    RewardAcnt (..),
    BootstrapAddress (..),
    fromBoostrapCompactAddress,
    compactAddr,
    decompactAddr,
    CompactAddr,
    unCompactAddr,
    isPayCredScriptCompactAddr,
    isBootstrapCompactAddr,
    decodeAddr,
    decodeAddrShort,
    decodeAddrEither,
    decodeAddrShortEither,
    fromCborAddr,
    fromCborBothAddr,
    fromCborCompactAddr,
    fromCborBackwardsBothAddr,
    decodeRewardAcnt,
    fromCborRewardAcnt,
  )
where

import qualified Cardano.Chain.Common as Byron (CompactAddress, unsafeGetCompactAddress)
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
  ( Addr (..),
    BootstrapAddress (..),
    RewardAcnt (..),
    byron,
    payCredIsScript,
    serialiseAddr,
  )
import Cardano.Ledger.BaseTypes
  ( CertIx (CertIx),
    Network (..),
    SlotNo (SlotNo),
    TxIx (TxIx),
    byronProtVer,
    natVersion,
  )
import Cardano.Ledger.Binary
  ( Decoder,
    FromCBOR (..),
    ToCBOR (..),
    decodeFull',
    ifDecoderVersionAtLeast,
  )
import Cardano.Ledger.Credential
  ( Credential (..),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Prelude (unsafeShortByteStringIndex)
import Control.DeepSeq (NFData)
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify', state)
import Data.Bits (Bits (clearBit, shiftL, testBit, (.&.), (.|.)))
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS (ShortByteString, fromShort, index, length, toShort)
import qualified Data.ByteString.Unsafe as BS (unsafeDrop, unsafeIndex)
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Show (intToDigit)
import GHC.Stack (HasCallStack)
import Numeric (showIntAtBase)

------------------------------------------------------------------------------------------
-- Compact Address -----------------------------------------------------------------------
------------------------------------------------------------------------------------------

newtype CompactAddr c = UnsafeCompactAddr ShortByteString
  deriving stock (Eq, Ord)
  deriving newtype (NFData)

instance Crypto c => Show (CompactAddr c) where
  show c = show (decompactAddr c)

-- | Unwrap the compact address and get to the address' binary representation.
unCompactAddr :: CompactAddr c -> ShortByteString
unCompactAddr (UnsafeCompactAddr sbs) = sbs

compactAddr :: Addr c -> CompactAddr c
compactAddr = UnsafeCompactAddr . SBS.toShort . serialiseAddr
{-# INLINE compactAddr #-}

decompactAddr :: forall c. (HasCallStack, Crypto c) => CompactAddr c -> Addr c
decompactAddr (UnsafeCompactAddr sbs) =
  case decodeAddrShortEither sbs of
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
fromCborAddr :: forall c s. Crypto c => Decoder s (Addr c)
fromCborAddr = fst <$> fromCborBothAddr
{-# INLINE fromCborAddr #-}

-- | Returns the actual bytes that represent an addres, while ensuring that they can
-- be decoded in any era as an `Addr` when need be.
fromCborCompactAddr :: forall c s. Crypto c => Decoder s (CompactAddr c)
fromCborCompactAddr = snd <$> fromCborBothAddr
{-# INLINE fromCborCompactAddr #-}

-- | This is the decoder for an address that returns both the actual `Addr` and the bytes,
-- that it was encoded as.
fromCborBothAddr :: forall c s. Crypto c => Decoder s (Addr c, CompactAddr c)
fromCborBothAddr = do
  sbs <- fromCBOR
  case decodeAddrShortEither sbs of
    Right addr -> pure (addr, UnsafeCompactAddr sbs)
    Left err ->
      ifDecoderVersionAtLeast (natVersion @7) (fail err) (decodeAddrDropSlack sbs)
  where
    -- Prior to Babbage era we did not check if a binary blob representing an address was
    -- fully consumed, so unfortunately we must preserve this behavior. However,
    -- decompacting of an address enforces that there are no bytes are left unconsumed,
    -- therefore we need to drop the garbage after we successfully decod the malformed
    -- address.
    decodeAddrDropSlack sbs = flip evalStateT 0 $ do
      addr <- decodeAddrStateAllowLeftoverT sbs
      bytesConsumed <- get
      let sbsCropped = SBS.toShort $ BS.take bytesConsumed $ SBS.fromShort sbs
      pure (addr, UnsafeCompactAddr sbsCropped)
{-# INLINE fromCborBothAddr #-}

fromCborBackwardsBothAddr ::
  forall c s.
  Crypto c =>
  Decoder s (Addr c, CompactAddr c)
fromCborBackwardsBothAddr = fromCborBothAddr
{-# DEPRECATED fromCborBackwardsBothAddr "Use `fromCborBothAddr` instead, they are the same" #-}

class AddressBuffer b where
  bufLength :: b -> Int

  bufUnsafeIndex :: b -> Int -> Word8

  bufToByteString :: b -> BS.ByteString

  bufGetHash :: Hash.HashAlgorithm h => b -> Int -> Maybe (Hash.Hash h a)

instance AddressBuffer ShortByteString where
  bufLength = SBS.length
  {-# INLINE bufLength #-}
  bufUnsafeIndex = unsafeShortByteStringIndex
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
  bufGetHash bs offset = do
    guard (offset >= 0 && offset < BS.length bs)
    Hash.hashFromBytes (BS.unsafeDrop offset bs)
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

newtype Fail a = Fail {runFail :: Either String a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Fail where
  fail = Fail . Left

decodeAddrEither ::
  forall c.
  Crypto c =>
  BS.ByteString ->
  Either String (Addr c)
decodeAddrEither sbs = runFail $ evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrEither #-}

decodeAddrShortEither ::
  forall c.
  Crypto c =>
  ShortByteString ->
  Either String (Addr c)
decodeAddrShortEither sbs = runFail $ evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrShortEither #-}

decodeAddrShort ::
  forall c m.
  (Crypto c, MonadFail m) =>
  ShortByteString ->
  m (Addr c)
decodeAddrShort sbs = evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrShort #-}

decodeAddr ::
  forall c m.
  (Crypto c, MonadFail m) =>
  BS.ByteString ->
  m (Addr c)
decodeAddr sbs = evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddr #-}

-- | While decoding an Addr the header (the first byte in the buffer) is
-- expected to be in a certain format. Here are the meaning of all the bits:
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
decodeAddrStateT ::
  (Crypto c, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (Addr c)
decodeAddrStateT buf = decodeAddrStateAllowLeftoverT buf <* ensureBufIsConsumed "Addr" buf
{-# INLINE decodeAddrStateT #-}

-- | Just like `decodeAddrStateT`, but does not check whether the input was consumed in full.
decodeAddrStateAllowLeftoverT ::
  (Crypto c, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (Addr c)
decodeAddrStateAllowLeftoverT buf = do
  guardLength "Header" 1 buf
  let header = Header $ bufUnsafeIndex buf 0
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
      staking <- decodeStakeReference header buf
      pure $ Addr (headerNetworkId header) payment staking
{-# INLINE decodeAddrStateAllowLeftoverT #-}

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
  forall c m b.
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (BootstrapAddress c)
decodeBootstrapAddress buf =
  case decodeFull' byronProtVer $ bufToByteString buf of
    Left e -> fail $ show e
    Right addr -> BootstrapAddress addr <$ modify' (+ bufLength buf)
{-# INLINE decodeBootstrapAddress #-}

decodePaymentCredential ::
  (Crypto c, MonadFail m, AddressBuffer b) =>
  Header ->
  b ->
  StateT Int m (PaymentCredential c)
decodePaymentCredential header buf
  | headerIsPaymentScript header = ScriptHashObj <$> decodeScriptHash buf
  | otherwise = KeyHashObj <$> decodeKeyHash buf
{-# INLINE decodePaymentCredential #-}

decodeStakeReference ::
  (Crypto c, MonadFail m, AddressBuffer b) =>
  Header ->
  b ->
  StateT Int m (StakeReference c)
decodeStakeReference header buf
  | headerIsBaseAddress header =
      if headerIsStakingScript header
        then StakeRefBase . ScriptHashObj <$> decodeScriptHash buf
        else StakeRefBase . KeyHashObj <$> decodeKeyHash buf
  | otherwise =
      if headerIsEnterpriseAddr header
        then pure StakeRefNull
        else StakeRefPtr <$> decodePtr buf
{-# INLINE decodeStakeReference #-}

decodeKeyHash ::
  (Crypto c, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (KeyHash kr c)
decodeKeyHash buf = KeyHash <$> decodeHash buf
{-# INLINE decodeKeyHash #-}

decodeScriptHash ::
  (Crypto c, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (ScriptHash c)
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
    <$> (SlotNo . (fromIntegral :: Word32 -> Word64) <$> decodeVariableLengthWord32 "SlotNo" buf)
    <*> (TxIx . (fromIntegral :: Word16 -> Word64) <$> decodeVariableLengthWord16 "TxIx" buf)
    <*> (CertIx . (fromIntegral :: Word16 -> Word64) <$> decodeVariableLengthWord16 "CertIx" buf)
{-# INLINE decodePtr #-}

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
        unless (bufUnsafeIndex buf off0 .&. 0b01111100 == 0) $
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
        unless (bufUnsafeIndex buf off0 .&. 0b01110000 == 0) $
          failDecoding name "More than 32bits was supplied"
        pure res
      {-# INLINE d7last #-}
  d7 (d7 (d7 (d7 d7last))) 0
{-# INLINE decodeVariableLengthWord32 #-}

------------------------------------------------------------------------------------------
-- Reward Account Deserializer -----------------------------------------------------------
------------------------------------------------------------------------------------------

decodeRewardAcnt ::
  forall c b m.
  (Crypto c, AddressBuffer b, MonadFail m) =>
  b ->
  m (RewardAcnt c)
decodeRewardAcnt buf = evalStateT (decodeRewardAccountT buf) 0
{-# INLINE decodeRewardAcnt #-}

fromCborRewardAcnt :: forall c s. Crypto c => Decoder s (RewardAcnt c)
fromCborRewardAcnt = do
  sbs :: ShortByteString <- fromCBOR
  decodeRewardAcnt @c sbs
{-# INLINE fromCborRewardAcnt #-}

headerIsRewardAccount :: Header -> Bool
headerIsRewardAccount header = header .&. 0b11101110 == 0b11100000
{-# INLINE headerIsRewardAccount #-}

headerRewardAccountIsScript :: Header -> Bool
headerRewardAccountIsScript = (`testBit` 4)
{-# INLINE headerRewardAccountIsScript #-}

-- | Reward Account Header.
--
-- @@@
--
-- ┏━━━━━━━━━━━━━━━━┳━┯━┯━┯━┯━┯━┯━┯━┓
-- ┃ Reward Account ┃1┊1┊1┊x┊0┊0┊0┊x┃
-- ┗━━━━━━━━━━━━━━━━╋━┿━┿━┿━┿━┿━┿━┿━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
--                  ┃1┊1┊1┊0┊0┊0┊0┊0┃ Testnet PaymentKey    StakingKey    ┃
--                  ┃1┊1┊1┊0┊0┊0┊0┊1┃ Mainnet PaymentKey    StakingKey    ┃
--                  ┃1┊1┊1┊1┊0┊0┊0┊0┃ Testnet PaymentScript StakingKey    ┃
--                  ┃1┊1┊1┊1┊0┊0┊0┊1┃ Mainnet PaymentScript StakingKey    ┃
--                  ┗━┷━┷━┷━┷━┷━┷━┷━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
--                          \       \
--                           \       `Is Mainnet Address
--                            `Account Credential is a Script
-- @@@
decodeRewardAccountT ::
  (MonadFail m, Crypto c, AddressBuffer b) =>
  b ->
  StateT Int m (RewardAcnt c)
decodeRewardAccountT buf = do
  guardLength "Header" 1 buf
  modify' (+ 1)
  let header = Header $ bufUnsafeIndex buf 0
  unless (headerIsRewardAccount header) $
    fail $
      "Invalid header for the reward account: " <> show header
  account <-
    if headerRewardAccountIsScript header
      then ScriptHashObj <$> decodeScriptHash buf
      else KeyHashObj <$> decodeKeyHash buf
  ensureBufIsConsumed "RewardsAcnt" buf
  pure $! RewardAcnt (headerNetworkId header) account
{-# INLINE decodeRewardAccountT #-}

instance Crypto c => ToCBOR (CompactAddr c) where
  toCBOR (UnsafeCompactAddr bytes) = toCBOR bytes
  {-# INLINE toCBOR #-}

instance Crypto c => FromCBOR (CompactAddr c) where
  fromCBOR = fromCborCompactAddr
  {-# INLINE fromCBOR #-}

-- | Efficiently check whether compated adddress is an address with a credential
-- that is a payment script.
isPayCredScriptCompactAddr :: CompactAddr c -> Bool
isPayCredScriptCompactAddr (UnsafeCompactAddr bytes) =
  testBit (SBS.index bytes 0) payCredIsScript

-- | Efficiently check whether compated adddress is a Byron address.
isBootstrapCompactAddr :: CompactAddr c -> Bool
isBootstrapCompactAddr (UnsafeCompactAddr bytes) = testBit (SBS.index bytes 0) byron

-- | Convert Byron's comapct address into `CompactAddr`. This is just an efficient type cast.
fromBoostrapCompactAddress :: Byron.CompactAddress -> CompactAddr c
fromBoostrapCompactAddress = UnsafeCompactAddr . Byron.unsafeGetCompactAddress
