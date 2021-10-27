{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE  RankNTypes  #-}
{-# LANGUAGE  ScopedTypeVariables  #-}
{-# LANGUAGE  TypeApplications  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Alonzo.TxOut where

import Data.ByteString(ByteString,pack,unpack,index)
import Cardano.Binary(serialize', FromCBOR (fromCBOR))
import qualified Data.ByteString as BS
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr(..), compactAddr, decompactAddr)
import Cardano.Ledger.BaseTypes(StrictMaybe(..),Network(..))
import Cardano.Ledger.Era(Era,Crypto)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Alonzo.Data(DataHash)
import Cardano.Ledger.SafeHash(SafeHash,extractHash,unsafeMakeSafeHash)
import Cardano.Crypto.Hash.Class(Hash(),hashToBytes,hashWith,HashAlgorithm,castHash, sizeHash, hashFromBytes)

import Cardano.Ledger.Address(Addr(..))
import Cardano.Ledger.Credential
  ( Credential(..),
    StakeReference(..))
import Cardano.Ledger.Hashes(ScriptHash(..))
import Cardano.Ledger.Keys(KeyHash(..))
import Cardano.Ledger.Mary.Value(Value(..))
import qualified Data.Map as Map
import Data.Word(Word8,Word64)
import Cardano.Ledger.Compactible (Compactible (..),CompactForm)
import qualified Cardano.Ledger.Crypto as CC
import Data.Maybe (fromJust)

import Data.Proxy
import qualified Codec.CBOR.Read as Read
import qualified Data.ByteString.Lazy as BSL

-- ==================================================
-- Getting to the ByteString that Underlies each Hash

safeHashToBytes :: SafeHash crypto a -> ByteString
safeHashToBytes = hashToBytes . extractHash

bytesToSafeHash ::  HashAlgorithm (CC.HASH c) => ByteString -> SafeHash c a
bytesToSafeHash x = unsafeMakeSafeHash (makeHash x)

makeHash :: HashAlgorithm c => ByteString -> Hash c a
makeHash bs = castHash (hashWith id bs)

-- ======================================================
makeTag :: Word8 -> Word8 -> Word8 -> Word8
makeTag addr val dhash
  | (addr >= 0 && addr <= 4) && (val >= 0 && val <= 2) && (dhash >= 0 && dhash <= 1) =
    (addr * 8 + val * 2 + dhash) -- (shiftL addr 3 .|. shiftL val 1 .|. dhash)
  | otherwise = error ("tags are not in the correct ranges "++show (addr,val,dhash) ++"in (0-4,0-2,0-1).")

getTags :: Word8 -> (Word8,Word8,Word8)
getTags tag = (addr,val,dhash)
  where dhash = mod tag 2
        val = mod (div tag 2) 4
        addr = mod (div tag 8) 8

-- ===============================================

encodeAddr :: Addr crypto -> Maybe (Word8, StakeReference crypto, ByteString)
encodeAddr (Addr Testnet (ScriptHashObj (ScriptHash hash1)) stake) = Just (0,stake,hashToBytes hash1)
encodeAddr (Addr Testnet (KeyHashObj (KeyHash hash1)) stake)       = Just (1,stake,hashToBytes hash1)
encodeAddr (Addr Mainnet (ScriptHashObj (ScriptHash hash1)) stake) = Just (2,stake,hashToBytes hash1)
encodeAddr (Addr Mainnet (KeyHashObj (KeyHash hash1)) stake)       = Just (3,stake,hashToBytes hash1)
encodeAddr (AddrBootstrap _)                                       = Nothing

decodeAddr :: forall crypto. HashAlgorithm (CC.ADDRHASH crypto) => Word8 -> Int -> StakeReference crypto -> ByteString -> (Int, Addr crypto)
decodeAddr tag byteIx stake bs = (byteIx + addrSizeBytes, addr)
  where
    addrSizeBytes = fromIntegral $ sizeHash (Proxy @(CC.ADDRHASH crypto))

    hash1 :: Hash (CC.ADDRHASH crypto) a
    hash1 = fromJust (hashFromBytes (subbytestring byteIx (byteIx + addrSizeBytes) bs))

    addr = case tag of
      0 -> Addr Testnet (ScriptHashObj (ScriptHash hash1)) stake
      1 -> Addr Testnet (KeyHashObj (KeyHash hash1)) stake
      2 -> Addr Mainnet (ScriptHashObj (ScriptHash hash1)) stake
      3 -> Addr Mainnet (KeyHashObj (KeyHash hash1)) stake
      _ -> error $ "CompactTxOut: Unexpected address tag: " <> show tag

encodeValue :: forall crypto. CC.Crypto crypto => Value crypto -> (Word8,ByteString)
encodeValue (Value 0 m) | Map.null m = (0, mempty)
encodeValue (Value n m) | Map.null m = (1, word64ToByteString (fromIntegral n))
encodeValue (v@(Value _ _))          = (2, serialize' @(CompactForm (Value crypto)) (unJust(toCompact v)))
    where unJust (Just x) = x
          unJust Nothing = error ("Value does not have compact form.")

decodeValue :: forall crypto. CC.Crypto crypto => Word8 -> Int -> ByteString -> (Int, Value crypto)
decodeValue tag byteIx bs = case tag of
  0 -> (byteIx, Value 0 Map.empty)
  1 -> (j, Value (fromIntegral n) Map.empty)
    where (j,n) = readWord64 byteIx bs
  2 -> case Read.deserialiseFromBytes @(CompactForm (Value crypto)) fromCBOR (BSL.fromStrict $ BS.drop byteIx bs) of
            Left _err -> error "CompactTxOut: Invalid Value encoding"
            Right (bsRest, cValue) -> (BS.length bs - fromIntegral (BSL.length bsRest), fromCompact cValue)
  _ -> error $ "CompactTxOut: Unexpected address tag: " <> show tag


encodeDataHash :: StrictMaybe (DataHash crypto) -> (Word8,ByteString)
encodeDataHash SNothing      = (0, mempty)
encodeDataHash (SJust mhash) = (1, safeHashToBytes mhash)

decodeDataHash :: forall crypto. HashAlgorithm (CC.HASH crypto) => Word8 -> Int -> ByteString -> (Int, StrictMaybe (DataHash crypto))
decodeDataHash tag i bs = case tag of
  0 -> (i, SNothing)
  1 -> (i + dataHashSizeBytes, SJust hash1)
    where
      dataHashSizeBytes = fromIntegral $ sizeHash (Proxy @(CC.HASH crypto))
      hash1 = unsafeMakeSafeHash $ fromJust (hashFromBytes (subbytestring i (i + dataHashSizeBytes) bs))
  _ -> error $ "CompactTxOut: Unexpected address tag: " <> show tag

subbytestring :: Int -> Int -> ByteString -> ByteString
subbytestring loInc hiExc = BS.drop loInc . BS.take (hiExc - loInc)

-- ===============================================

data TxOut era
  = TxOutCompactShelley !(StakeReference (Crypto era)) !ByteString -- TODO change to a SBS then to a unpacked representation
  | TxOutCompact
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value (Crypto era))) -- TODO change to Core.Value
  | TxOutCompactDH
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value (Crypto era))) -- TODO change to Core.Value
      !(DataHash (Crypto era))


compactTxOut ::
  ( Era era,
    -- Show(Core.Value era),
    Core.Value era ~ Value (Crypto era)
  ) => Addr (Crypto era) -> Value (Crypto era) -> StrictMaybe (DataHash (Crypto era)) -> TxOut era
compactTxOut addr val dhashMay = case encodeAddr addr of
  Nothing -> case dhashMay of
    SNothing -> TxOutCompact (compactAddr addr) (fromJust (toCompact val))
    SJust dhash -> TxOutCompactDH (compactAddr addr) (fromJust (toCompact val)) dhash
  Just (addrTag, stake, addrBytes)
      -> TxOutCompactShelley
          stake
          (tagBytes <> addrBytes <> valueBytes <> dhashBytes)
    where (dhashTag, dhashBytes) = encodeDataHash dhashMay
          (valueTag, valueBytes) = encodeValue val
          tagBytes = pack [makeTag addrTag valueTag dhashTag]

decompactTxOut :: forall era.
  ( Era era,
    Core.Value era ~ Value (Crypto era)
  ) => TxOut era -> (Addr (Crypto era), Value (Crypto era), StrictMaybe (DataHash (Crypto era)))
decompactTxOut (TxOutCompactShelley stake bytes) =  (addr, val, dhash)
  where (i1,(addrtag,valtag,dhashtag)) = readTags 0 bytes
        (i2,addr) = decodeAddr @(Crypto era) addrtag i1 stake bytes
        (i3,val) = decodeValue @(Crypto era) valtag i2 bytes
        (_i4,dhash) = decodeDataHash @(Crypto era) dhashtag i3 bytes
decompactTxOut (TxOutCompact cAddr cVal) = (decompactAddr cAddr, fromCompact cVal, SNothing)
decompactTxOut (TxOutCompactDH cAddr cVal dh) = (decompactAddr cAddr, fromCompact cVal, SJust dh)

-- =============================================
showBS :: ByteString -> String
showBS bs = show(unpack bs)

word64ToByteString :: Word64 -> ByteString
word64ToByteString w64 = pack(loop 8 w64 [])
  where loop :: Word64 -> Word64 -> [Word8] -> [Word8]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n 256) ((fromIntegral (mod n 256)):ans)

readWord8:: Int -> ByteString -> (Int,Word8)
readWord8 i bs | i > (BS.length bs - 1) = error ("Not enough bytes to read a Word8")
readWord8 i bs = (i+1,index bs i)

readTags:: Int -> ByteString -> (Int,(Word8,Word8,Word8))
readTags i bs | i > (BS.length bs - 1) = error ("Not enough bytes to read the Tags")
readTags i bs = (i+1,getTags(index bs i))

readWord64:: Int -> ByteString -> (Int,Word64)
readWord64 i bs | i+7 > BS.length bs = error ("Not enough bytes to read a Word64")
readWord64 i bs = (i+8,loop 0 0)
  where loop :: Int -> Word64 -> Word64
        loop j ans | j >= 8 = ans
        loop j ans = loop (j+1) (ans * 256 + fromIntegral(index bs j))

-- | Read a (sub) ByteString of length 'len', starting at index 'i' from 'bs'
readByteString:: Int -> Int -> ByteString -> (Int,ByteString)
readByteString len i bs | i+len > BS.length bs =
    error ("Not enough bytes to read a ByteString of length "++show len)
readByteString len i bs = (i+len,BS.take len (BS.drop i bs))

-- TODO replace variouse `div` with `shiftR`
-- TODO review `fromJust`s and replace with error messages