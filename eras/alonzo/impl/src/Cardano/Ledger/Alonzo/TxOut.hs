{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Alonzo.TxOut
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH)
  )
  where

import Data.ByteString(ByteString,pack,index)
import Cardano.Binary(serialize', FromCBOR (..), ToCBOR (..), encodeListLen, decodeListLenOrIndef, decodeBreakOr, DecoderError (DecoderErrorCustom))
import qualified Data.ByteString as BS
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr(..), compactAddr, decompactAddr)
import Cardano.Ledger.BaseTypes(StrictMaybe(..),Network(..))
import Cardano.Ledger.Era(Era,Crypto)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Alonzo.Data(DataHash)
import Cardano.Ledger.SafeHash(SafeHash,extractHash,unsafeMakeSafeHash)
import Cardano.Crypto.Hash.Class(Hash(),hashToBytes,HashAlgorithm, sizeHash, hashFromBytes)

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
import GHC.Stack (HasCallStack)
import NoThunks.Class
import GHC.Records (HasField(..))
import Cardano.Ledger.Val
import Cardano.Prelude (cborError)

data TxOut era
  = TxOutCompactShelley
      !(StakeReference (Crypto era))
      {-# UNPACK #-} !ByteString -- TODO change to a SBS then to a unpacked representation
  | TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value (Crypto era))) -- TODO change to Core.Value
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Value (Crypto era))) -- TODO change to Core.Value
      !(DataHash (Crypto era))

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

instance (Era era, CC.Crypto (Crypto era), Core.Value era ~ Value (Crypto era))
  => Eq (TxOut era) where
  TxOutCompact' cAddrA cValA == TxOutCompact' cAddrB cValB
    = cAddrA == cAddrB
    && cValA == cValB
  TxOutCompactDH' cAddrA cValA dhA == TxOutCompactDH' cAddrB cValB dhB
    = cAddrA == cAddrB
    && cValA == cValB
    && dhA == dhB
  TxOut addrA valA dhMayA == TxOut addrB valB dhMayB
    = addrA == addrB
    && valA == valB
    && dhMayA == dhMayB

instance
  ( Era era,
    Core.Value era ~ Value (Crypto era), -- TODO remove
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

pattern TxOut ::
  ( Era era,
    Compactible (Core.Value era),
    HasCallStack,
    Core.Value era ~ Value (Crypto era) -- TODO Remove
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  StrictMaybe (DataHash (Crypto era)) ->
  TxOut era
pattern TxOut addr vl dh <- (viewTxOut -> (addr, vl, dh))
  where
    TxOut addr vl mdh = compactTxOut addr vl mdh

{-# COMPLETE TxOut #-}

-- TODO deprecate in favour of TxOut
pattern TxOutCompact ::
  ( Era era,
    Compactible (Core.Value era),
    HasCallStack,
    Core.Value era ~ Value (Crypto era) -- TODO Remove
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Value (Crypto era)) ->
  TxOut era
pattern TxOutCompact addr vl <- (viewCompactTxOut -> (addr, vl, SNothing))
  where
    TxOutCompact = TxOutCompact'

-- TODO deprecate in favour of TxOut
pattern TxOutCompactDH ::
  ( Era era,
    Compactible (Core.Value era),
    HasCallStack,
    Core.Value era ~ Value (Crypto era) -- TODO Remove
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Value (Crypto era)) ->
  DataHash (Crypto era) ->
  TxOut era
pattern TxOutCompactDH addr vl dh <- (viewCompactTxOut -> (addr, vl, SJust dh))
  where
    TxOutCompactDH = TxOutCompactDH'

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ==================================================
-- Getting to the ByteString that Underlies each Hash

safeHashToBytes :: SafeHash crypto a -> ByteString
safeHashToBytes = hashToBytes . extractHash

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

-- TODO
-- TODO We use a CBOR encoding for multi-asset values. I'm not sure if
-- TODO that'll really save space or perhaps make it worse!
-- TODO
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

compactTxOut ::
  ( Era era,
    -- Show(Core.Value era),
    Core.Value era ~ Value (Crypto era)
  ) => Addr (Crypto era) -> Value (Crypto era) -> StrictMaybe (DataHash (Crypto era)) -> TxOut era
compactTxOut addr val dhashMay = case encodeAddr addr of
  Nothing -> case dhashMay of
    SNothing -> TxOutCompact' (compactAddr addr) (fromJust (toCompact val))
    SJust dhash -> TxOutCompactDH' (compactAddr addr) (fromJust (toCompact val)) dhash
  Just (addrTag, stake, addrBytes)
      -> TxOutCompactShelley
          stake
          (tagBytes <> addrBytes <> valueBytes <> dhashBytes)
    where (dhashTag, dhashBytes) = encodeDataHash dhashMay
          (valueTag, valueBytes) = encodeValue val
          tagBytes = pack [makeTag addrTag valueTag dhashTag]

viewTxOut :: forall era.
  ( Era era,
    Core.Value era ~ Value (Crypto era)
  ) => TxOut era -> (Addr (Crypto era), Value (Crypto era), StrictMaybe (DataHash (Crypto era)))
viewTxOut (TxOutCompactShelley stake bytes) =  (addr, val, dhash)
  where (i1,(addrtag,valtag,dhashtag)) = readTags 0 bytes
        (i2,addr) = decodeAddr @(Crypto era) addrtag i1 stake bytes
        (i3,val) = decodeValue @(Crypto era) valtag i2 bytes
        (_i4,dhash) = decodeDataHash @(Crypto era) dhashtag i3 bytes
viewTxOut (TxOutCompact' cAddr cVal) = (decompactAddr cAddr, fromCompact cVal, SNothing)
viewTxOut (TxOutCompactDH' cAddr cVal dh) = (decompactAddr cAddr, fromCompact cVal, SJust dh)

viewCompactTxOut :: forall era.
  ( Era era,
    Core.Value era ~ Value (Crypto era)
  ) => TxOut era -> (CompactAddr (Crypto era), CompactForm (Value (Crypto era)), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut (TxOutCompact' cAddr cVal) = (cAddr, cVal, SNothing)
viewCompactTxOut (TxOutCompactDH' cAddr cVal dh) = (cAddr, cVal, SJust dh)
viewCompactTxOut txOut@TxOutCompactShelley{} = (compactAddr addr, fromJust (toCompact val), dhash)
  where
  (addr, val, dhash) = viewTxOut txOut

-- =============================================

word64ToByteString :: Word64 -> ByteString
word64ToByteString w64 = pack(loop 8 w64 [])
  where loop :: Word64 -> Word64 -> [Word8] -> [Word8]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n 256) ((fromIntegral (mod n 256)):ans)

readTags:: Int -> ByteString -> (Int,(Word8,Word8,Word8))
readTags i bs | i > (BS.length bs - 1) = error ("Not enough bytes to read the Tags")
readTags i bs = (i+1,getTags(index bs i))

readWord64:: Int -> ByteString -> (Int,Word64)
readWord64 i bs | i+7 > BS.length bs = error ("Not enough bytes to read a Word64")
readWord64 i bs = (i+8,loop 0 0)
  where loop :: Int -> Word64 -> Word64
        loop j ans | j >= 8 = ans
        loop j ans = loop (j+1) (ans * 256 + fromIntegral(index bs j))

-- TODO replace variouse `div` with `shiftR`
-- TODO review `fromJust`s and replace with error messages

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era,
    Compactible (Core.Value era),
    Core.Value era ~ Value (Crypto era) -- TODO remove
  ) =>
  ToCBOR (TxOut era)
  where
  toCBOR txOut = case viewCompactTxOut txOut of
    (cAddr, cVal, dhMay) -> case dhMay of
      SNothing -> encodeListLen 2
        <> toCBOR @(CompactAddr (Crypto era)) cAddr
        <> toCBOR @(CompactForm (Core.Value era)) cVal
      SJust dh -> encodeListLen 3
        <> toCBOR @(CompactAddr (Crypto era)) cAddr
        <> toCBOR @(CompactForm (Core.Value era)) cVal
        <> toCBOR @(DataHash (Crypto era)) dh

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    Compactible (Core.Value era),
    Core.Value era ~ Value (Crypto era) -- TODO remove
  ) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = do
    lenOrIndef <- decodeListLenOrIndef
    case lenOrIndef of
      Nothing -> do
        a <- fromCBOR
        cv <- decodeNonNegative
        decodeBreakOr >>= \case
          True -> pure $ TxOutCompact' a cv
          False -> do
            dh <- fromCBOR
            decodeBreakOr >>= \case
              True -> pure $ TxOutCompactDH' a cv dh
              False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
      Just 2 ->
        TxOut
          <$> fromCBOR
          <*> decodeNonNegative
          <*> pure SNothing
      Just 3 ->
        TxOutCompactDH'
          <$> fromCBOR
          <*> decodeNonNegative
          <*> fromCBOR
      Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"

-- TODO do we still need this?
instance (Crypto era ~ c
  , Era era, Core.Value era ~ Value c -- TODO remove
  ) => HasField "compactAddress" (TxOut era) (CompactAddr c) where
  getField (TxOutCompact' a _) = a
  getField (TxOutCompactDH' a _ _) = a
  getField (TxOut a _ _) = compactAddr a

instance (CC.Crypto c, Crypto era ~ c
  , Era era, Core.Value era ~ Value c -- TODO remove
  ) => HasField "address" (TxOut era) (Addr c) where
  getField (TxOut a _ _) = a

instance (Core.Value era ~ val, Compactible val
  , Era era, Core.Value era ~ Value (Crypto era) -- TODO remove
  ) => HasField "value" (TxOut era) val where
  getField (TxOut _ v _) = v

instance (c ~ Crypto era
  , Era era, Core.Value era ~ Value c -- TODO remove
  ) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField (TxOut _ _ dhMay) = dhMay