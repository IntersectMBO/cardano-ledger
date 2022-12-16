{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Core.Address
  ( -- * Exported for benchmarking only
    fromCborCompactAddrOld,
    decompactAddrLazy,
    getShortAddr,
    decompactAddrOld,
  )
where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address
  ( Word7 (..),
    byron,
    getAddr,
    isEnterpriseAddr,
    notBaseAddr,
    payCredIsScript,
    stakeCredIsScript,
    toWord7,
    word7sToWord64,
  )
import Cardano.Ledger.BaseTypes (CertIx (..), SlotNo (..), TxIx (..), word8ToNetwork)
import Cardano.Ledger.Binary
  ( Decoder,
    DecoderError (DecoderErrorCustom),
    FromCBOR (fromCBOR),
    byronProtVer,
    cborError,
    decodeFull',
  )
import Cardano.Ledger.CompactAddress
  ( Addr (..),
    BootstrapAddress (BootstrapAddress),
    CompactAddr (..),
  )
import Cardano.Ledger.Credential
  ( Credential (..),
    PaymentCredential,
    Ptr (..),
    StakeReference (..),
  )
import Cardano.Ledger.Crypto (ADDRHASH, Crypto)
import Cardano.Ledger.Hashes (ScriptHash (ScriptHash))
import Cardano.Ledger.Keys (KeyHash (..))
import Control.Monad (ap)
import qualified Control.Monad.Fail
import qualified Data.Binary.Get as B
import Data.Bits (Bits (testBit, (.&.)))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short as SBS (ShortByteString, fromShort, index, length)
import Data.ByteString.Short.Internal as SBS (ShortByteString (SBS))
import Data.Maybe (fromMaybe)
import qualified Data.Primitive.ByteArray as BA
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.Word

------------------------------------------------------------------------------------------
-- Old Compact Address Deserializer --------------------------------------------------------------
------------------------------------------------------------------------------------------

-- This is a fallback deserializer that preserves old behavior. It will almost never be
-- invoked, that is why it is not inlined.
fromCborAddrOld :: Crypto c => ShortByteString -> Decoder s (Addr c)
fromCborAddrOld sbs =
  case B.runGetOrFail getAddr $ BSL.fromStrict $ SBS.fromShort sbs of
    Right (_remaining, _offset, value) -> pure value
    Left (_remaining, _offset, message) ->
      cborError (DecoderErrorCustom "Addr" $ fromString message)
{-# NOINLINE fromCborAddrOld #-}

fromCborCompactAddrOld :: forall s c. Crypto c => Decoder s (CompactAddr c)
fromCborCompactAddrOld = do
  sbs <- fromCBOR
  UnsafeCompactAddr sbs <$ fromCborAddrOld @c sbs
{-# INLINE fromCborCompactAddrOld #-}

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

getBootstrapAddress :: GetShort (BootstrapAddress c)
getBootstrapAddress = do
  bs <- getRemainingAsByteString
  case decodeFull' byronProtVer bs of
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
  Ptr
    <$> (SlotNo <$> getVariableLengthWord64)
    <*> (TxIx . fromIntegral <$> getVariableLengthWord64)
    <*> (CertIx . fromIntegral <$> getVariableLengthWord64)

getKeyHash :: Crypto c => GetShort (Credential kr c)
getKeyHash = KeyHashObj . KeyHash <$> getHash

getScriptHash :: Crypto c => GetShort (Credential kr c)
getScriptHash = ScriptHashObj . ScriptHash <$> getHash

getStakeReference :: Crypto c => Word8 -> GetShort (StakeReference c)
getStakeReference header = case testBit header notBaseAddr of
  True -> case testBit header isEnterpriseAddr of
    True -> pure StakeRefNull
    False -> StakeRefPtr <$> getPtr
  False -> case testBit header stakeCredIsScript of
    True -> StakeRefBase <$> getScriptHash
    False -> StakeRefBase <$> getKeyHash

getPayCred :: Crypto c => Word8 -> GetShort (PaymentCredential c)
getPayCred header = case testBit header payCredIsScript of
  True -> getScriptHash
  False -> getKeyHash

getShortAddr :: Crypto c => GetShort (Addr c)
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

decompactAddrOld :: Crypto c => ShortByteString -> Addr c
decompactAddrOld short = snd . unwrap "CompactAddr" $ runGetShort getShortAddr 0 short
  where
    -- The reason failure is impossible here is that the only way to call this code
    -- is using a CompactAddr, which can only be constructed using compactAddr.
    -- compactAddr serializes an Addr, so this is guaranteed to work.
    unwrap :: forall a. Text -> Maybe a -> a
    unwrap name = fromMaybe (error $ unpack $ "Impossible failure when decoding " <> name)
{-# NOINLINE decompactAddrOld #-}

-- | This lazy deserializer is kept around purely for benchmarking, so we can
-- verify that new deserializer `decodeAddrStateT` is doing the work lazily.
decompactAddrLazy :: forall c. Crypto c => CompactAddr c -> Addr c
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
    unwrap name = fromMaybe (error $ unpack $ "Impossible failure when decoding " <> name)
    header = run "address header" 0 bytes getWord
    addrNetId =
      unwrap "address network id" $
        word8ToNetwork $
          header .&. 0x0F -- 0b00001111 is the mask for the network id
          -- The address format is
          -- header | pay cred | stake cred
          -- where the header is 1 byte
          -- the pay cred is (sizeHash (ADDRHASH crypto)) bytes
          -- and the stake cred can vary
    paycred = run "payment credential" 1 bytes (getPayCred header)
    stakecred = run "staking credential" 1 bytes $ do
      skipHash ([] @(ADDRHASH c))
      getStakeReference header
    skipHash :: forall proxy h. Hash.HashAlgorithm h => proxy h -> GetShort ()
    skipHash p = skip . fromIntegral $ Hash.sizeHash p
    skip :: Int -> GetShort ()
    skip n = GetShort $ \i sbs ->
      let offsetStop = i + n
       in if offsetStop <= SBS.length sbs
            then Just (offsetStop, ())
            else Nothing
