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
import Cardano.Ledger.BaseTypes (CertIx (..), TxIx (..), word8ToNetwork)
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
import Cardano.Prelude (Text, cborError, panic)
import Control.DeepSeq (NFData)
import Control.Monad (ap)
import qualified Control.Monad.Fail
import Control.Monad.Trans.State.Strict
import Data.Bits (testBit, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Short as SBS
import Data.ByteString.Short.Internal as SBS (ShortByteString (SBS), unsafeIndex)
import Data.Maybe (fromMaybe)
import Data.Primitive.ByteArray as BA
import Data.Word (Word64, Word8)

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
decompactAddr = undefined

data EncoderError =
  EncoderError
    { encoderErrorOffset :: Int
    , encoderErrorMessage :: String
    }
  deriving (Eq, Show)

type EncoderT m a = StateT Int (ExceptT (EncoderError String) m) a

class Encode a where
  decodeByteArray :: ByteArray -> EncoderT Indentity a

  decodeByteString :: ByteString -> EncoderT Identity a

  encodeByteArray :: MutableByteArray s -> a -> EncoderT (ST s) ()

  encodePtr :: Ptr b -> a -> EncoderT IO ()

decodeAddr ::
  forall crypto m.
  (CC.Crypto crypto, MonadFail m) =>
  ShortByteString ->
  StateT Int m (Addr crypto)
decodeAddr sbs = do
  let n = SBS.length sbs
  guard (n > 1)
  header :: Word8 <- SBS.unsafeIndex sbs 0
  if testBit header byron
    then AddrBootstrap <$> decodeBootstrapAddress ba
    else do
      let addrNetId = header .&. 0x0F -- 0b00001111 is the mask for the network id
      case word8ToNetwork addrNetId of
        Just n -> do
          c <- decodePaymentCredential sbs header
          h <- decodePaymentStakeReference sbs header
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

decodePaymentCredential ::
  CC.Crypto crypto =>
  ShortByteString ->
  Word8 ->
  Maybe (PaymentCredential crypto)
decodePaymentCredential header
  | testBit header payCredIsScript = getScriptHash
  | otherwise = getKeyHash

-- | Efficiently check whether compated adddress is an address with a credential
-- that is a payment script.
isPayCredScriptCompactAddr :: CompactAddr crypto -> Bool
isPayCredScriptCompactAddr (UnsafeCompactAddr bytes) =
  testBit (SBS.index bytes 0) payCredIsScript

-- | Efficiently check whether compated adddress is a Byron address.
isBootstrapCompactAddr :: CompactAddr crypto -> Bool
isBootstrapCompactAddr (UnsafeCompactAddr bytes) = testBit (SBS.index bytes 0) byron
