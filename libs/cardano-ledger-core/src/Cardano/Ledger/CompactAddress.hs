{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.CompactAddress
  ( compactAddr,
    decompactAddr,
    CompactAddr (..),
    substring,
    isBootstrapRedeemer,
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
import Cardano.Ledger.BaseTypes (word8ToNetwork)
import Cardano.Ledger.Credential
  ( Credential (KeyHashObj, ScriptHashObj),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (ADDRHASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Prelude (Text, cborError, panic)
import Control.Monad (ap)
import qualified Control.Monad.Fail
import Data.Bits (testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Maybe (fromMaybe)
import qualified Data.Primitive.ByteArray as BA
import Data.Word (Word64, Word8)

newtype CompactAddr crypto = UnsafeCompactAddr ShortByteString
  deriving (Eq, Ord)

instance CC.Crypto c => Show (CompactAddr c) where
  show c = show (decompactAddr c)

compactAddr :: Addr crypto -> CompactAddr crypto
compactAddr = UnsafeCompactAddr . SBS.toShort . serialiseAddr

decompactAddr :: forall crypto. CC.Crypto crypto => CompactAddr crypto -> Addr crypto
decompactAddr (UnsafeCompactAddr bytes) =
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

instance CC.Crypto crypto => ToCBOR (CompactAddr crypto) where
  toCBOR (UnsafeCompactAddr bytes) = toCBOR bytes

instance CC.Crypto crypto => FromCBOR (CompactAddr crypto) where
  fromCBOR = do
    sbs <- fromCBOR
    case deserializeShortAddr @crypto sbs of
      Just _ -> pure $ UnsafeCompactAddr sbs
      Nothing -> cborError $ DecoderErrorCustom "Addr" "invalid address"

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

deserializeShortAddr :: CC.Crypto crypto => ShortByteString -> Maybe (Addr crypto)
deserializeShortAddr short = snd <$> runGetShort getShortAddr 0 short

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

getRemainingAsByteString :: GetShort ByteString
getRemainingAsByteString = GetShort $ \i sbs ->
  let l = SBS.length sbs
   in if i < l
        then Just (l, SBS.fromShort $ substring sbs i l)
        else Nothing

skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> GetShort ()
skipHash p = skip . fromIntegral $ Hash.sizeHash p

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

skip :: Int -> GetShort ()
skip n = GetShort $ \i sbs ->
  let offsetStop = i + n
   in if offsetStop <= SBS.length sbs
        then Just (offsetStop, ())
        else Nothing

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
    <*> getVariableLengthWord64
    <*> getVariableLengthWord64

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

-- | WARNING: This optimized version of isBootstrapRedeemer does not agree
-- with the one in Cardano.Ledger.Address
isBootstrapRedeemer :: CompactAddr crypto -> Bool
isBootstrapRedeemer (UnsafeCompactAddr bytes) =
  testBit header byron -- AddrBootstrap
    && addrType == 2 -- ATRedeem
  where
    addrType = SBS.index bytes (SBS.length bytes - 6)
    header = SBS.index bytes 0
