{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.CompactAddress
  ( compactAddr,
    decompactAddr,
    CompactAddr (..),
    substring,
    isPayCredScriptCompactAddr,
    isBootstrapCompactAddr,
    -- Faster Address deserialization
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

    -- * Exported for benchmarking only
    fromCborCompactAddrOld,
    decompactAddrLazy,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeFull',
  )
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
  ( Addr (..),
    BootstrapAddress (..),
    RewardAcnt (..),
    Word7 (..),
    byron,
    getAddr,
    isEnterpriseAddr,
    notBaseAddr,
    payCredIsScript,
    serialiseAddr,
    stakeCredIsScript,
    toWord7,
    word7sToWord64,
  )
import Cardano.Ledger.BaseTypes (CertIx (..), Network (..), TxIx (..), word8ToNetwork)
import Cardano.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (ADDRHASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Prelude (panic)
import Control.DeepSeq (NFData)
import Control.Monad (ap, guard, unless, when)
import qualified Control.Monad.Fail
import Control.Monad.Trans.State (StateT, evalStateT, get, modify', state)
import qualified Data.Binary.Get as B
import Data.Bits (Bits, clearBit, shiftL, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short as SBS (fromShort, index, length, toShort)
import Data.ByteString.Short.Internal as SBS (ShortByteString (SBS), unsafeIndex)
import qualified Data.ByteString.Unsafe as BS (unsafeDrop, unsafeIndex)
import Data.Coders (cborError)
import Data.Maybe (fromMaybe)
import qualified Data.Primitive.ByteArray as BA
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Show (intToDigit)
import Numeric (showIntAtBase)

newtype CompactAddr crypto = UnsafeCompactAddr ShortByteString
  deriving (Eq, Ord, NFData)

instance CC.Crypto c => Show (CompactAddr c) where
  show c = show (decompactAddr c)

compactAddr :: Addr crypto -> CompactAddr crypto
compactAddr = UnsafeCompactAddr . SBS.toShort . serialiseAddr
{-# INLINE compactAddr #-}

decompactAddr :: forall crypto. CC.Crypto crypto => CompactAddr crypto -> Addr crypto
decompactAddr (UnsafeCompactAddr sbs) =
  case decodeAddrShort sbs of
    Just addr -> addr
    Nothing -> decompactAddrOld sbs
{-# INLINE decompactAddr #-}

decompactAddrOld :: CC.Crypto crypto => ShortByteString -> Addr crypto
decompactAddrOld short = snd . unwrap "CompactAddr" $ runGetShort getShortAddr 0 short
  where
    -- The reason failure is impossible here is that the only way to call this code
    -- is using a CompactAddr, which can only be constructed using compactAddr.
    -- compactAddr serializes an Addr, so this is guaranteed to work.
    unwrap :: forall a. Text -> Maybe a -> a
    unwrap name = fromMaybe (panic $ "Impossible failure when decoding " <> name)
{-# NOINLINE decompactAddrOld #-}

------------------------------------------------------------------------------------------
-- Fast Address Serializer ---------------------------------------------------------------
------------------------------------------------------------------------------------------

fromCborAddr :: forall crypto s. CC.Crypto crypto => Decoder s (Addr crypto)
fromCborAddr = do
  sbs <- fromCBOR
  decodeAddrShort @crypto sbs
{-# INLINE fromCborAddr #-}

fromCborBothAddr :: forall crypto s. CC.Crypto crypto => Decoder s (Addr crypto, CompactAddr crypto)
fromCborBothAddr = do
  sbs <- fromCBOR
  addr <- decodeAddrShort @crypto sbs
  pure (addr, UnsafeCompactAddr sbs)
{-# INLINE fromCborBothAddr #-}

fromCborCompactAddr :: forall crypto s. CC.Crypto crypto => Decoder s (CompactAddr crypto)
fromCborCompactAddr = do
  -- Ensure bytes can be decoded as Addr
  (_addr, cAddr) <- fromCborBothAddr
  pure cAddr
{-# INLINE fromCborCompactAddr #-}

-- This is a fallback deserializer that preserves old behavior. It will almost never be
-- invoked, that is why it is not inlined.
fromCborAddrFallback :: CC.Crypto crypto => ShortByteString -> Decoder s (Addr crypto)
fromCborAddrFallback sbs =
  case B.runGetOrFail getAddr $ BSL.fromStrict $ SBS.fromShort sbs of
    Right (_remaining, _offset, value) -> pure value
    Left (_remaining, _offset, message) ->
      cborError (DecoderErrorCustom "Addr" $ fromString message)
{-# NOINLINE fromCborAddrFallback #-}

fromCborCompactAddrOld :: forall s crypto. CC.Crypto crypto => Decoder s (CompactAddr crypto)
fromCborCompactAddrOld = do
  sbs <- fromCBOR
  UnsafeCompactAddr sbs <$ fromCborAddrFallback @crypto sbs
{-# INLINE fromCborCompactAddrOld #-}

fromCborBackwardsBothAddr ::
  forall crypto s.
  CC.Crypto crypto =>
  Decoder s (Addr crypto, CompactAddr crypto)
fromCborBackwardsBothAddr = do
  sbs <- fromCBOR
  addr <-
    case decodeAddrShortEither @crypto sbs of
      Right a -> pure a
      Left _err -> fromCborAddrFallback sbs
  pure (addr, UnsafeCompactAddr sbs)
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
  bufGetHash bs offset = do
    guard (offset >= 0 && offset < BS.length bs)
    Hash.hashFromBytes (BS.unsafeDrop offset bs)
  {-# INLINE bufGetHash #-}

-- | Address header byte truth table:
newtype Header = Header Word8
  deriving (Eq, Ord, Bits, Num)

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
  deriving (Functor, Applicative, Monad)

instance MonadFail Fail where
  fail = Fail . Left

decodeAddrEither ::
  forall crypto.
  CC.Crypto crypto =>
  BS.ByteString ->
  Either String (Addr crypto)
decodeAddrEither sbs = runFail $ evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrEither #-}

decodeAddrShortEither ::
  forall crypto.
  CC.Crypto crypto =>
  ShortByteString ->
  Either String (Addr crypto)
decodeAddrShortEither sbs = runFail $ evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrShortEither #-}

decodeAddrShort ::
  forall crypto m.
  (CC.Crypto crypto, MonadFail m) =>
  ShortByteString ->
  m (Addr crypto)
decodeAddrShort sbs = evalStateT (decodeAddrStateT sbs) 0
{-# INLINE decodeAddrShort #-}

decodeAddr ::
  forall crypto m.
  (CC.Crypto crypto, MonadFail m) =>
  BS.ByteString ->
  m (Addr crypto)
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
  (CC.Crypto crypto, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (Addr crypto)
decodeAddrStateT buf = do
  guardLength "Header" 1 buf
  let header = Header $ bufUnsafeIndex buf 0
  addr <-
    if isByronAddress header
      then AddrBootstrap <$> decodeBootstrapAddress buf
      else do
        -- Ensure there are no unexpected bytes in the header
        unless (header .&. headerNonShelleyBits == 0) $
          failDecoding
            "Shelley Address"
            $ "Invalid header. Unused bits are not suppose to be set: " <> show header
        -- Advance one byte for the consumed header
        modify' (+ 1)
        payment <- decodePaymentCredential header buf
        staking <- decodeStakeReference header buf
        pure $ Addr (headerNetworkId header) payment staking
  addr <$ ensureBufIsConsumed "Addr" buf
{-# INLINE decodeAddrStateT #-}

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
    failDecoding name $ "Left over bytes: " ++ show (len - lastOffset)
{-# INLINE ensureBufIsConsumed #-}

-- | This decoder assumes the whole `ShortByteString` is occupied by the `BootstrapAddress`
decodeBootstrapAddress ::
  forall crypto m b.
  (MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (BootstrapAddress crypto)
decodeBootstrapAddress buf =
  case decodeFull' $ bufToByteString buf of
    Left e -> fail $ show e
    Right addr -> BootstrapAddress addr <$ modify' (+ bufLength buf)
{-# INLINE decodeBootstrapAddress #-}

decodePaymentCredential ::
  (CC.Crypto crypto, MonadFail m, AddressBuffer b) =>
  Header ->
  b ->
  StateT Int m (PaymentCredential crypto)
decodePaymentCredential header buf
  | headerIsPaymentScript header = ScriptHashObj <$> decodeScriptHash buf
  | otherwise = KeyHashObj <$> decodeKeyHash buf
{-# INLINE decodePaymentCredential #-}

decodeStakeReference ::
  (CC.Crypto crypto, MonadFail m, AddressBuffer b) =>
  Header ->
  b ->
  StateT Int m (StakeReference crypto)
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
  (CC.Crypto crypto, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (KeyHash kr crypto)
decodeKeyHash buf = KeyHash <$> decodeHash buf
{-# INLINE decodeKeyHash #-}

decodeScriptHash ::
  (CC.Crypto crypto, MonadFail m, AddressBuffer b) =>
  b ->
  StateT Int m (ScriptHash crypto)
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
      d7last :: Word32 -> StateT Int m Word32
      d7last acc = do
        res <- decode7BitVarLength name buf (\_ -> failDecoding name "too many bytes.") acc
        -- Only while decoding the last 7bits we check if there was too many
        -- bits supplied at the beginning.
        unless (bufUnsafeIndex buf off0 .&. 0b01110000 == 0) $
          failDecoding name "More than 32bits was supplied"
        pure res
  d7 (d7 (d7 (d7 d7last))) 0
{-# INLINE decodeVariableLengthWord32 #-}

------------------------------------------------------------------------------------------
-- Reward Account Deserializer -----------------------------------------------------------
------------------------------------------------------------------------------------------

decodeRewardAcnt ::
  forall crypto b m.
  (CC.Crypto crypto, AddressBuffer b, MonadFail m) =>
  b ->
  m (RewardAcnt crypto)
decodeRewardAcnt buf = evalStateT (decodeRewardAccountT buf) 0
{-# INLINE decodeRewardAcnt #-}

fromCborRewardAcnt :: forall crypto s. CC.Crypto crypto => Decoder s (RewardAcnt crypto)
fromCborRewardAcnt = do
  sbs :: ShortByteString <- fromCBOR
  decodeRewardAcnt @crypto sbs
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
  (MonadFail m, CC.Crypto crypto, AddressBuffer b) =>
  b ->
  StateT Int m (RewardAcnt crypto)
decodeRewardAccountT buf = do
  guardLength "Header" 1 buf
  modify' (+ 1)
  let header = Header $ bufUnsafeIndex buf 0
  unless (headerIsRewardAccount header) $
    fail $ "Invalid header for the reward account: " <> show header
  account <-
    if headerRewardAccountIsScript header
      then ScriptHashObj <$> decodeScriptHash buf
      else KeyHashObj <$> decodeKeyHash buf
  ensureBufIsConsumed "RewardsAcnt" buf
  pure $! RewardAcnt (headerNetworkId header) account
{-# INLINE decodeRewardAccountT #-}

------------------------------------------------------------------------------------------
-- Old Address Deserializer --------------------------------------------------------------
------------------------------------------------------------------------------------------

-- | This lazy deserializer is kept around purely for benchmarking, so we can
-- verify that new deserializer `decodeAddrStateT` is doing the work lazily.
decompactAddrLazy :: forall crypto. CC.Crypto crypto => CompactAddr crypto -> Addr crypto
decompactAddrLazy (UnsafeCompactAddr bytes) =
  if testBit header byron
    then AddrBootstrap $ run "byron address" 0 bytes getBootstrapAddress
    else Addr addrNetId paycred stakecred
  where
    run :: forall a. Text -> Int -> ShortByteString -> GetShort a -> a
    run name i sbs g = snd . unwrap name $ runGetShort g i sbs
    -- The reason failure is impossible here is that the only way to call this code
    -- is using a CompactAddr, which can only be constructed using compactAddr.
    -- compactAddr serializes an Addr, so this is guaranteed to work.
    unwrap :: forall a. Text -> Maybe a -> a
    unwrap name = fromMaybe (panic $ "Impossible failure when decoding " <> name)
    header = run "address header" 0 bytes getWord
    addrNetId =
      unwrap "address network id" $
        word8ToNetwork $ header .&. 0x0F -- 0b00001111 is the mask for the network id
        -- The address format is
        -- header | pay cred | stake cred
        -- where the header is 1 byte
        -- the pay cred is (sizeHash (ADDRHASH crypto)) bytes
        -- and the stake cred can vary
    paycred = run "payment credential" 1 bytes (getPayCred header)
    stakecred = run "staking credential" 1 bytes $ do
      skipHash ([] @(ADDRHASH crypto))
      getStakeReference header
    skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> GetShort ()
    skipHash p = skip . fromIntegral $ Hash.sizeHash p
    skip :: Int -> GetShort ()
    skip n = GetShort $ \i sbs ->
      let offsetStop = i + n
       in if offsetStop <= SBS.length sbs
            then Just (offsetStop, ())
            else Nothing

instance CC.Crypto crypto => ToCBOR (CompactAddr crypto) where
  toCBOR (UnsafeCompactAddr bytes) = toCBOR bytes

instance CC.Crypto crypto => FromCBOR (CompactAddr crypto) where
  fromCBOR = do
    (_addr, cAddr) <- fromCborBackwardsBothAddr
    pure cAddr
  {-# INLINE fromCBOR #-}

newtype GetShort a = GetShort {runGetShort :: Int -> ShortByteString -> Maybe (Int, a)}
  deriving (Functor)

instance Applicative GetShort where
  pure a = GetShort $ \i _sbs -> Just (i, a)
  (<*>) = ap

instance Monad GetShort where
  (GetShort g) >>= f = GetShort $ \i sbs ->
    case g i sbs of
      Nothing -> Nothing
      Just (i', x) -> runGetShort (f x) i' sbs

instance Control.Monad.Fail.MonadFail GetShort where
  fail _ = GetShort $ \_ _ -> Nothing

getShortAddr :: CC.Crypto crypto => GetShort (Addr crypto)
getShortAddr = do
  header <- peekWord8
  if testBit header byron
    then AddrBootstrap <$> getBootstrapAddress
    else do
      _ <- getWord -- read past the header byte
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> do
          c <- getPayCred header
          h <- getStakeReference header
          pure (Addr n c h)
        Nothing ->
          fail $
            concat
              ["Address with unknown network Id. (", show addrNetId, ")"]

getBootstrapAddress :: GetShort (BootstrapAddress crypto)
getBootstrapAddress = do
  bs <- getRemainingAsByteString
  case decodeFull' bs of
    Left e -> fail $ show e
    Right r -> pure $ BootstrapAddress r

getWord :: GetShort Word8
getWord = GetShort $ \i sbs ->
  if i < SBS.length sbs
    then Just (i + 1, SBS.index sbs i)
    else Nothing

peekWord8 :: GetShort Word8
peekWord8 = GetShort peek
  where
    peek i sbs = if i < SBS.length sbs then Just (i, SBS.index sbs i) else Nothing

getRemainingAsByteString :: GetShort BS.ByteString
getRemainingAsByteString = GetShort $ \i sbs ->
  let l = SBS.length sbs
   in if i < l
        then Just (l, SBS.fromShort $ substring sbs i l)
        else Nothing

getHash :: forall a h. Hash.HashAlgorithm h => GetShort (Hash.Hash h a)
getHash = GetShort $ \i sbs ->
  let hashLen = Hash.sizeHash ([] @h)
      offsetStop = i + fromIntegral hashLen
   in if offsetStop <= SBS.length sbs
        then do
          hash <- Hash.hashFromBytesShort $ substring sbs i offsetStop
          Just (offsetStop, hash)
        else Nothing

-- start is the first index copied
-- stop is the index after the last index copied
substring :: ShortByteString -> Int -> Int -> ShortByteString
substring (SBS ba) start stop =
  case BA.cloneByteArray (BA.ByteArray ba) start (stop - start) of
    BA.ByteArray ba' -> SBS ba'

getWord7s :: GetShort [Word7]
getWord7s = do
  next <- getWord
  -- is the high bit set?
  if testBit next 7
    then -- if so, grab more words
      (:) (toWord7 next) <$> getWord7s
    else -- otherwise, this is the last one
      pure [Word7 next]

getVariableLengthWord64 :: GetShort Word64
getVariableLengthWord64 = word7sToWord64 <$> getWord7s

getPtr :: GetShort Ptr
getPtr =
  Ptr <$> (SlotNo <$> getVariableLengthWord64)
    <*> (TxIx . fromIntegral <$> getVariableLengthWord64)
    <*> (CertIx . fromIntegral <$> getVariableLengthWord64)

getKeyHash :: CC.Crypto crypto => GetShort (Credential kr crypto)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getScriptHash :: CC.Crypto crypto => GetShort (Credential kr crypto)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getStakeReference :: CC.Crypto crypto => Word8 -> GetShort (StakeReference crypto)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

getPayCred :: CC.Crypto crypto => Word8 -> GetShort (PaymentCredential crypto)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

-- | Efficiently check whether compated adddress is an address with a credential
-- that is a payment script.
isPayCredScriptCompactAddr :: CompactAddr crypto -> Bool
isPayCredScriptCompactAddr (UnsafeCompactAddr bytes) =
  testBit (SBS.index bytes 0) payCredIsScript

-- | Efficiently check whether compated adddress is a Byron address.
isBootstrapCompactAddr :: CompactAddr crypto -> Bool
isBootstrapCompactAddr (UnsafeCompactAddr bytes) = testBit (SBS.index bytes 0) byron
