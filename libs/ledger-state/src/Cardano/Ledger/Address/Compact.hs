{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Address.Compact
  ( compactAddr,
    decompactAddr,
    decodeAddr,
    decodeAddrEither,
    decodeAddrShortEither,
    fromCborAddr,
    CompactAddr (..),
    fromCborCompactAddr,
    decodeRewardAccount,
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
    isEnterpriseAddr,
    notBaseAddr,
    payCredIsScript,
    serialiseAddr,
    stakeCredIsScript,
    toWord7,
    word7sToWord64,
  )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CompactAddress (CompactAddr (..))
import Cardano.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Crypto (ADDRHASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal as SBS (unsafeIndex)
import Data.ByteString.Unsafe as BS (unsafeDrop, unsafeIndex)
import Data.Proxy
import Data.Word

-- | Problems needs solving:
--
-- * Same encoder/decoder for CBOR and Compact
-- * Sticking Coin inside Addr28Extra can save more than it already does

-- newtype CompactAddr crypto = UnsafeCompactAddr ShortByteString
--   deriving (Eq, Ord, NFData)

-- instance CC.Crypto c => Show (CompactAddr c) where
--   show c = show (decompactAddr c)

compactAddr :: Addr crypto -> CompactAddr crypto
compactAddr = undefined

decompactAddr :: forall crypto. CC.Crypto crypto => CompactAddr crypto -> Addr crypto
decompactAddr (UnsafeCompactAddr sbs) = maybe (error "Impossible") id (decodeAddrShort sbs)


fromCborAddr :: forall crypto s. CC.Crypto crypto => Decoder s (Addr crypto)
fromCborAddr = do
  sbs <- fromCBOR
  decodeAddrShort @crypto sbs
{-# INLINE fromCborAddr #-}

fromCborCompactAddr :: forall crypto s. CC.Crypto crypto => Decoder s (CompactAddr crypto)
fromCborCompactAddr = do
  sbs <- fromCBOR
  -- Ensure bytes can be decoded as Addr
  _ <- decodeAddrShort @crypto sbs
  pure $ UnsafeCompactAddr sbs
{-# INLINE fromCborCompactAddr #-}

class AddressBuffer b where
  bufLength :: b -> Int

  bufUnsafeIndex :: b -> Int -> Word8

  bufToByteString :: b -> BS.ByteString

  bufGetHash :: Hash.HashAlgorithm h => b -> Int -> Maybe (Hash.Hash h a)

instance AddressBuffer ShortByteString where
  bufLength = SBS.length
  bufUnsafeIndex = SBS.unsafeIndex
  bufToByteString = SBS.fromShort
  bufGetHash = Hash.hashFromOffsetBytesShort

instance AddressBuffer BS.ByteString where
  bufLength = BS.length
  bufUnsafeIndex = BS.unsafeIndex
  bufToByteString = id
  bufGetHash bs offset = do
    guard (offset >= 0 && offset < BS.length bs)
    Hash.hashFromBytes (BS.unsafeDrop offset bs)

-- | Address header byte truth table:
newtype Header = Header Word8
  deriving (Eq, Ord, Show, Bits, Num)

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
          failDecoding "Shelley Address" "Invalid header. Unused bits are not suppose to be set."
        -- Advance one byte for the consumed header
        modify' (+ 1)
        payment <- decodePaymentCredential header buf
        staking <- decodeStakeReference header buf
        pure $ Addr (headerNetworkId header) payment staking
  addr <$ ensureBufIsConsumed "Addr" buf
{-# INLINEABLE decodeAddrStateT #-}

-- | Checks that the current offset is exactly at the end of the buffer.
ensureBufIsConsumed ::
  forall m b.
  (MonadFail m, AddressBuffer b) =>
  -- | Name for error reporting
  String ->
  -- | Buffer that should have beeen consumed.
  b ->
  StateT Int m ()
ensureBufIsConsumed name buf = do
  lastOffset <- get
  let len = bufLength buf
  unless (lastOffset == len) $
    failDecoding name $ "Left over bytes: " ++ show (len - lastOffset)

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
  Ptr <$> (SlotNo . (fromIntegral :: Word32 -> Word64) <$> decodeVariableLengthWord32 "SlotNo" buf)
    <*> (TxIx <$> decodeVariableLengthWord16 "TxIx" buf)
    <*> (CertIx <$> decodeVariableLengthWord16 "CertIx" buf)
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
        -- bits suuplied at the beginning.
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
        -- bits suuplied at the beginning.
        unless (bufUnsafeIndex buf off0 .&. 0b01110000 == 0) $
          failDecoding name "More than 32bits was supplied"
        pure res
  d7 (d7 (d7 (d7 d7last))) 0
{-# INLINE decodeVariableLengthWord32 #-}

------------------------------------------------------------------------------------------
-- Reward Account ------------------------------------------------------------------------
------------------------------------------------------------------------------------------

decodeRewardAccount ::
  (MonadFail m, Crypto crypto, AddressBuffer b) => b -> m (RewardAcnt crypto)
decodeRewardAccount buf = evalStateT (decodeRewardAccountT buf) 0
{-# INLINE decodeRewardAccount #-}

headerIsRewardAccount :: Header -> Bool
headerIsRewardAccount header = header .&. 0b11101110 == 0x11100000
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
  (MonadFail m, Crypto crypto, AddressBuffer b) =>
  b ->
  StateT Int m (RewardAcnt crypto)
decodeRewardAccountT buf = do
  guardLength "Header" 1 buf
  modify' (+ 1)
  let header = Header $ bufUnsafeIndex buf 0
  unless (headerIsRewardAccount header) $
    fail "Invalid header for the reward account"
  account <-
    if headerRewardAccountIsScript header
      then ScriptHashObj <$> decodeScriptHash buf
      else KeyHashObj <$> decodeKeyHash buf
  ensureBufIsConsumed "RewardsAcnt" buf
  pure $! RewardAcnt (headerNetworkId header) account
{-# INLINEABLE decodeRewardAccountT #-}
