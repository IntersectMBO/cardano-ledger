{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Address.Compact
  ( compactAddr,
    decompactAddr,
    CompactAddr (..),
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeFull',
  )
import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
  ( Addr (..),
    BootstrapAddress (..),
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
import Cardano.Ledger.BaseTypes -- (CertIx (..), TxIx (..), word8ToNetwork)
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
-- import Cardano.Prelude (Text, cborError, panic)
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad (ap)
import qualified Control.Monad.Fail
import Control.Monad.Trans.State.Strict
import Data.Bits
import Data.Bits (testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal as SBS (ShortByteString (SBS), unsafeIndex)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray as BA
import Data.Proxy
import Data.Word

-- | Problems needs solving:
--
-- * Same encoder/decoder for ShortByteString and ByteString
-- * Same encoder/decoder for CBOR and Compact
-- * Sticking Coin inside Addr28Extra can save more than it already does
newtype CompactAddr crypto = UnsafeCompactAddr ShortByteString
  deriving (Eq, Ord, NFData)

instance CC.Crypto c => Show (CompactAddr c) where
  show c = show (decompactAddr c)

compactAddr :: Addr crypto -> CompactAddr crypto
compactAddr = undefined

decompactAddr :: forall crypto. CC.Crypto crypto => CompactAddr crypto -> Addr crypto
decompactAddr (UnsafeCompactAddr sbs) = maybe (error "Impossible") id (decodeAddr sbs)

-- data EncoderError = EncoderError
--   { encoderErrorOffset :: Int,
--     encoderErrorMessage :: String
--   }
--   deriving (Eq, Show)

-- type EncoderT m a = StateT Int (ExceptT (EncoderError String) m) a

-- class Encode a where
--   decodeByteArray :: ByteArray -> EncoderT Indentity a

--   decodeByteString :: ByteString -> EncoderT Identity a

--   encodeByteArray :: MutableByteArray s -> a -> EncoderT (ST s) ()

--   encodePtr :: Ptr b -> a -> EncoderT IO ()

-- | Address header byte truth table:
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
newtype Header = Header Word8
  deriving (Eq, Ord, Show, Bits, Num)

-- | Every Byron address starts with @[TkListLen 2]@, which encodes as 130 (or 0x80)
headerByron :: Header
headerByron = 0b10000010 -- 0x80

isByronAddress :: Header -> Bool
isByronAddress = (== headerByron)

headerNonShelleyBits :: Header
headerNonShelleyBits = headerByron .|. 0b00001100

headerNetworkId :: Header -> Network
headerNetworkId header
  | header `testBit` 0 = Mainnet
  | otherwise = Testnet

headerIsPaymentScript :: Header -> Bool
headerIsPaymentScript = (`testBit` 4)

headerIsEnterpriseAddr :: Header -> Bool
headerIsEnterpriseAddr = (`testBit` 5)

headerIsStakingScript :: Header -> Bool
headerIsStakingScript = (`testBit` 5)

headerIsBaseAddress :: Header -> Bool
headerIsBaseAddress = not . (`testBit` 6)


-- newtype FailT m a = FailT { runFailT :: m (Either String a)}
--   deriving (Eq, Ord, Show, Functor, Applicative, Monad)

-- runFail = runIdentity . runFailT

decodeAddr ::
  forall crypto m.
  (CC.Crypto crypto, MonadFail m) =>
  ShortByteString ->
  m (Addr crypto)
decodeAddr sbs = evalStateT (decodeAddrStateT sbs) 0

decodeAddrStateT ::
  forall crypto m.
  (CC.Crypto crypto, MonadFail m) =>
  ShortByteString ->
  StateT Int m (Addr crypto)
decodeAddrStateT sbs = do
  guardLength "Header" 1 sbs
  let header = Header $ SBS.unsafeIndex sbs 0
  addr <-
    if isByronAddress header
      then AddrBootstrap <$> decodeBootstrapAddress sbs
      else do
        -- Ensure there are no unexpected bytes in the header
        unless (header .&. headerNonShelleyBits == 0) $
          failDecoding "Shelley Address" "Invalid header. Unused bits are not suppose to be set."
        -- Advance one byte for the consumed header
        modify' (+ 1)
        payment <- decodePaymentCredential header sbs
        staking <- decodeStakeReference header sbs
        pure $ Addr (headerNetworkId header) payment staking
  lastOffset <- get
  let len = SBS.length sbs
  unless (lastOffset == len) $
    fail $ "Left over bytes: " ++ show (len - lastOffset)
  pure addr

-- | This decoder assumes the whole `ShortByteString` is occupied by the `BootstrapAddress`
decodeBootstrapAddress ::
     forall crypto m. MonadFail m
  => ShortByteString
  -> StateT Int m (BootstrapAddress crypto)
decodeBootstrapAddress sbs =
  case decodeFull' $ SBS.fromShort sbs of
    Left e -> fail $ show e
    Right addr -> BootstrapAddress addr <$ modify' (+ SBS.length sbs)

decodePaymentCredential ::
  (CC.Crypto crypto, MonadFail m) =>
  Header ->
  ShortByteString ->
  StateT Int m (PaymentCredential crypto)
decodePaymentCredential header sbs
  | headerIsPaymentScript header = ScriptHashObj <$> decodeScriptHash sbs
  | otherwise = KeyHashObj <$> decodeKeyHash sbs

decodeStakeReference ::
  (CC.Crypto crypto, MonadFail m) =>
  Header ->
  ShortByteString ->
  StateT Int m (StakeReference crypto)
decodeStakeReference header sbs
  | headerIsBaseAddress header =
      if headerIsStakingScript header
        then StakeRefBase . ScriptHashObj <$> decodeScriptHash sbs
        else StakeRefBase . KeyHashObj <$> decodeKeyHash sbs
  | otherwise =
      if headerIsEnterpriseAddr header
        then pure StakeRefNull
        else StakeRefPtr <$> decodePtr sbs

decodeKeyHash ::
     (CC.Crypto crypto, MonadFail m)
  => ShortByteString
  -> StateT Int m (KeyHash kr crypto)
decodeKeyHash sbs = KeyHash <$> decodeHash sbs

decodeScriptHash ::
     (CC.Crypto crypto, MonadFail m)
  => ShortByteString
  -> StateT Int m (ScriptHash crypto)
decodeScriptHash sbs = ScriptHash <$> decodeHash sbs

decodeHash ::
  forall a h m.
  (Hash.HashAlgorithm h, MonadFail m) =>
  ShortByteString ->
  StateT Int m (Hash.Hash h a)
decodeHash sbs = do
  offset <- get
  case Hash.hashFromOffsetBytesShort sbs offset of
    Just h -> h <$ modify' (+ hashLen)
    Nothing
      | offset >= 0 ->
          failDecoding "Hash" $
            "Not enough bytes supplied: "
              ++ show (SBS.length sbs - offset)
              ++ ". Expected: "
              ++ show hashLen
    Nothing -> fail "Impossible: Negative offset"
  where
    hashLen :: Int
    hashLen = fromIntegral (Hash.sizeHash (Proxy :: Proxy h))

decodePtr ::
     forall m. MonadFail m
  => ShortByteString
  -> StateT Int m Ptr
decodePtr sbs =
  Ptr <$> (SlotNo . (fromIntegral :: Word32 -> Word64) <$> decodeVariableLengthWord32 "SlotNo" sbs)
      <*> (TxIx <$> decodeVariableLengthWord16 "TxIx" sbs)
      <*> (CertIx <$> decodeVariableLengthWord16 "CertIx" sbs)

guardLength ::
     forall m. MonadFail m
  => String
  -- ^ Name for what is being decoded for the error message
  -> Int
  -> ShortByteString
  -> StateT Int m ()
guardLength name expectedLength sbs = do
  offset <- get
  when (offset > SBS.length sbs - expectedLength) $
    failDecoding name "Not enough bytes for decoding"

-- decodeWord8 ::
--      forall m. MonadFail m
--   => String
--   -> ShortByteString
--   -> StateT Int m Word8
-- decodeWord8 name sbs = do
--   guardLength name 1 sbs
--   offset <- get
--   SBS.unsafeIndex sbs offset <$ put (offset + 1)


-- | Decode a variable length integral value that is encoded with 7 bits of data
-- and the most significant bit (MSB), the 8th bit is set whenever there are
-- more bits following. Continuation style allows us to avoid
-- rucursion. Removing loops is good for performance.
decode7BitVarLength ::
     forall a m. (Num a, Bits a, MonadFail m)
  => String
  -- ^ Name of what is being decoded for error reporting
  -> ShortByteString
  -- ^ Buffer that contains encoded number
  -> (a -> StateT Int m a)
  -- ^ Continuation that will be invoked if MSB is set
  -> a
  -- ^ Accumulator
  -> StateT Int m a
decode7BitVarLength name sbs cont !acc = do
  guardLength name 1 sbs
  offset <- state (\off -> (off, off + 1))
  let b8 = SBS.unsafeIndex sbs offset
  if b8 `testBit` 7
    then cont (acc `shiftL` 7 .|. fromIntegral (b8 `clearBit` 7))
    else pure (acc `shiftL` 7 .|. fromIntegral b8)

failDecoding :: MonadFail m => String -> String -> m a
failDecoding name msg = fail $ "Decoding " ++ name ++ ": " ++ msg

decodeVariableLengthWord16 ::
     forall m. MonadFail m
  => String
  -> ShortByteString
  -> StateT Int m Word16
decodeVariableLengthWord16 name sbs = do
  off0 <- get
  let d7 = decode7BitVarLength name sbs
      d7last :: Word16 -> StateT Int m Word16
      d7last acc = do
        res <- decode7BitVarLength name sbs (\_ -> failDecoding name "too many bytes.") acc
        -- Only while decoding the last 7bits we check if there was too many
        -- bits suuplied at the beginning.
        unless (SBS.unsafeIndex sbs off0 .&. 0b01111100 == 0) $
          failDecoding name "More than 16bits was supplied"
        pure res
  d7 (d7 d7last) 0

decodeVariableLengthWord32 ::
     forall m. MonadFail m
  => String
  -> ShortByteString
  -> StateT Int m Word32
decodeVariableLengthWord32 name sbs = do
  off0 <- get
  let d7 = decode7BitVarLength name sbs
      d7last :: Word32 -> StateT Int m Word32
      d7last acc = do
        res <- decode7BitVarLength name sbs (\_ -> failDecoding name "too many bytes.") acc
        -- Only while decoding the last 7bits we check if there was too many
        -- bits suuplied at the beginning.
        unless (SBS.unsafeIndex sbs off0 .&. 0b01110000 == 0) $
          failDecoding name "More than 32bits was supplied"
        pure res
  d7 (d7 (d7 (d7 d7last))) 0

-- getBootstrapAddress :: GetShort (BootstrapAddress crypto)
-- getBootstrapAddress = do
--   bs <- getRemainingAsByteString
--   case decodeFull' bs of
--     Left e -> fail $ show e
--     Right r -> pure $ BootstrapAddress r

-- getWord :: GetShort Word8
-- getWord = GetShort $ \i sbs ->
--   if i < SBS.length sbs
--     then Just (i + 1, SBS.index sbs i)
--     else Nothing

-- peekWord8 :: GetShort Word8
-- peekWord8 = GetShort peek
--   where
--     peek i sbs = if i < SBS.length sbs then Just (i, SBS.index sbs i) else Nothing

-- getRemainingAsByteString :: GetShort ByteString
-- getRemainingAsByteString = GetShort $ \i sbs ->
--   let l = SBS.length sbs
--    in if i < l
--         then Just (l, SBS.fromShort $ substring sbs i l)
--         else Nothing

-- skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> GetShort ()
-- skipHash p = skip . fromIntegral $ Hash.sizeHash p

-- -- start is the first index copied
-- -- stop is the index after the last index copied
-- substring :: ShortByteString -> Int -> Int -> ShortByteString
-- substring (SBS ba) start stop =
--   case BA.cloneByteArray (BA.ByteArray ba) start (stop - start) of
--     BA.ByteArray ba' -> SBS ba'

-- skip :: Int -> GetShort ()
-- skip n = GetShort $ \i sbs ->
--   let offsetStop = i + n
--    in if offsetStop <= SBS.length sbs
--         then Just (offsetStop, ())
--         else Nothing

-- getWord7s :: GetShort [Word7]
-- getWord7s = do
--   next <- getWord
--   -- is the high bit set?
--   if testBit next 7
--     then -- if so, grab more words
--       (:) (toWord7 next) <$> getWord7s
--     else -- otherwise, this is the last one
--       pure [Word7 next]

-- getVariableLengthWord64 :: GetShort Word64
-- getVariableLengthWord64 = word7sToWord64 <$> getWord7s


-- -- | Efficiently check whether compated adddress is an address with a credential
-- -- that is a payment script.
-- isPayCredScriptCompactAddr :: CompactAddr crypto -> Bool
-- isPayCredScriptCompactAddr (UnsafeCompactAddr bytes) =
--   testBit (SBS.index bytes 0) payCredIsScript

-- -- | Efficiently check whether compated adddress is a Byron address.
-- isBootstrapCompactAddr :: CompactAddr crypto -> Bool
-- isBootstrapCompactAddr (UnsafeCompactAddr bytes) = testBit (SBS.index bytes 0) byron
