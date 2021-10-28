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
import Cardano.Binary(FromCBOR (..), ToCBOR (..), encodeListLen, decodeListLenOrIndef, decodeBreakOr, DecoderError (DecoderErrorCustom))
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
import Data.Word(Word8,Word64)
import Cardano.Ledger.Compactible (Compactible (..),CompactForm)
import qualified Cardano.Ledger.Crypto as CC
import Data.Maybe (fromJust)

import Data.Proxy
import GHC.Stack (HasCallStack)
import NoThunks.Class
import GHC.Records (HasField(..))
import Cardano.Ledger.Val
import Cardano.Prelude (cborError)
import Cardano.Ledger.Coin (Coin(..))

data TxOut era
  = TxOutCompactShelleyAdaOnly
      !(StakeReference (Crypto era))
      {-# UNPACK #-} !ByteString -- TODO change to a SBS then to a unpacked representation
  | TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era)) -- TODO change to Core.Value
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era)) -- TODO change to Core.Value
      !(DataHash (Crypto era))

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

instance Era era => Eq (TxOut era) where
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
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

pattern TxOut ::
  ( Era era,
    -- Compactible (Core.Value era),
    -- Show (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  StrictMaybe (DataHash (Crypto era)) ->
  TxOut era
pattern TxOut addr vl dh <- (viewTxOut -> (addr, vl, dh))
  where
    TxOut addr vl mdh = mkTxOut addr vl mdh

{-# COMPLETE TxOut #-}

-- TODO deprecate in favour of TxOut
pattern TxOutCompact ::
  ( Era era,
    Compactible (Core.Value era),
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  TxOut era
pattern TxOutCompact addr vl <- (viewCompactTxOut -> (addr, vl, SNothing))
  where
    TxOutCompact = TxOutCompact'

-- TODO deprecate in favour of TxOut
pattern TxOutCompactDH ::
  ( Era era,
    Compactible (Core.Value era),
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
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
makeTag :: Word8 -> Word8 -> Word8
makeTag addr dhash
  | (addr >= 0 && addr <= 4) && (dhash >= 0 && dhash <= 1) =
    (addr * 2 + dhash) -- (shiftL addr 1 .|. dhash)
  | otherwise = error ("tags are not in the correct ranges "++show (addr, dhash) ++"in (0-4, 0-1).")

getTags :: Word8 -> (Word8,Word8)
getTags tag = (addr,dhash)
  where dhash = mod tag 2
        addr = mod (div tag 2) 4

-- ===============================================

encodeAddr :: forall crypto. Addr crypto -> Maybe (Word8, StakeReference crypto, ByteString)
encodeAddr (Addr Testnet (ScriptHashObj (ScriptHash hash1)) stake) = Just (0, stake, hashToBytes hash1)
encodeAddr (Addr Testnet (KeyHashObj (KeyHash hash1)) stake)       = Just (1, stake, hashToBytes hash1)
encodeAddr (Addr Mainnet (ScriptHashObj (ScriptHash hash1)) stake) = Just (2, stake, hashToBytes hash1)
encodeAddr (Addr Mainnet (KeyHashObj (KeyHash hash1)) stake)       = Just (3, stake, hashToBytes hash1)
encodeAddr (AddrBootstrap _)                                       = Nothing

decodeAddr :: forall crypto. HashAlgorithm (CC.ADDRHASH crypto) => Word8 -> Int -> StakeReference crypto -> ByteString -> (Int, Addr crypto)
decodeAddr tag byteIx stake bs = (byteIx + addrSizeBytes, addr)
  where
    addrSizeBytes = fromIntegral $ sizeHash (Proxy @(CC.ADDRHASH crypto))

    hash1 :: Hash (CC.ADDRHASH crypto) a
    hash1 = fromJust (hashFromBytes (subbytestring byteIx addrSizeBytes bs))

    addr = case tag of
      0 -> Addr Testnet (ScriptHashObj (ScriptHash hash1)) stake
      1 -> Addr Testnet (KeyHashObj (KeyHash hash1)) stake
      2 -> Addr Mainnet (ScriptHashObj (ScriptHash hash1)) stake
      3 -> Addr Mainnet (KeyHashObj (KeyHash hash1)) stake
      _ -> error $ "CompactTxOut: Unexpected address tag: " <> show tag

encodeValue :: Val (Core.Value era) => Proxy era -> Core.Value era -> Maybe ByteString
encodeValue _ v = if adaOnly v
  then Just $ word64ToByteString $ fromIntegral $ unCoin $ coin v
  else Nothing

decodeValue :: Val (Core.Value era) => Proxy era -> Int -> ByteString -> (Int, Core.Value era)
decodeValue _ byteIx bs = (j, inject (Coin (fromIntegral n)))
  where (j,n) = readWord64 byteIx bs


encodeDataHash :: StrictMaybe (DataHash crypto) -> (Word8,ByteString)
encodeDataHash SNothing      = (0, mempty)
encodeDataHash (SJust mhash) = (1, safeHashToBytes mhash)

decodeDataHash :: forall crypto. HashAlgorithm (CC.HASH crypto) => Word8 -> Int -> ByteString -> (Int, StrictMaybe (DataHash crypto))
decodeDataHash tag i bs = case tag of
  0 -> (i, SNothing)
  1 -> (i + dataHashSizeBytes, SJust hash1)
    where
      dataHashSizeBytes = fromIntegral $ sizeHash (Proxy @(CC.HASH crypto))
      hash1 = unsafeMakeSafeHash $ fromJust (hashFromBytes (subbytestring i dataHashSizeBytes bs))
  _ -> error $ "CompactTxOut: Unexpected address tag: " <> show tag

subbytestring :: Int -> Int -> ByteString -> ByteString
subbytestring loInc sz bs
  | BS.length bs' /= sz = error $ "subbytestring " <> show loInc <> " " <> show sz <> " bs: out of bounds (length bs == " <> show (BS.length bs) <> ")"
  | otherwise = bs'
  where
    bs' = BS.take sz $ BS.drop loInc bs

-- ===============================================

mkTxOut ::
  forall era.
  ( Era era,
    HasCallStack
  ) => Addr (Crypto era) -> Core.Value era -> StrictMaybe (DataHash (Crypto era)) -> TxOut era
mkTxOut addr val dhashMay = case (encodeAddr @(Crypto era) addr, encodeValue (Proxy @era) val) of
  (Just (addrTag, stake, addrBytes), Just valueBytes)
      -> TxOutCompactShelleyAdaOnly
          stake
          (tagBytes <> addrBytes <> valueBytes <> dhashBytes)
    where (dhashTag, dhashBytes) = encodeDataHash dhashMay
          tagBytes = pack [makeTag addrTag dhashTag]
  _ -> case dhashMay of
    SNothing -> TxOutCompact' (compactAddr addr) (fromJust (toCompact val))
    SJust dhash -> TxOutCompactDH' (compactAddr addr) (fromJust (toCompact val)) dhash

viewTxOut :: forall era.
  ( Era era
  ) => TxOut era -> (Addr (Crypto era), Core.Value era, StrictMaybe (DataHash (Crypto era)))
viewTxOut (TxOutCompactShelleyAdaOnly stake bytes) =  (addr, val, dhash)
  where (i1,(addrtag, dhashtag)) = readTags 0 bytes
        (i2,addr) = decodeAddr @(Crypto era) addrtag i1 stake bytes
        (i3,val) = decodeValue (Proxy @era) i2 bytes
        (_i4,dhash) = decodeDataHash @(Crypto era) dhashtag i3 bytes
viewTxOut (TxOutCompact' cAddr cVal) = (decompactAddr cAddr, fromCompact cVal, SNothing)
viewTxOut (TxOutCompactDH' cAddr cVal dh) = (decompactAddr cAddr, fromCompact cVal, SJust dh)

viewCompactTxOut :: forall era.
  ( Era era
  ) => TxOut era -> (CompactAddr (Crypto era), CompactForm (Core.Value era), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut (TxOutCompact' cAddr cVal) = (cAddr, cVal, SNothing)
viewCompactTxOut (TxOutCompactDH' cAddr cVal dh) = (cAddr, cVal, SJust dh)
viewCompactTxOut txOut@TxOutCompactShelleyAdaOnly{} = (compactAddr addr, fromJust (toCompact val), dhash)
  where
  (addr, val, dhash) = viewTxOut txOut

-- =============================================

word64ToByteString :: Word64 -> ByteString
word64ToByteString w64 = pack(loop 8 w64 [])
  where loop :: Word64 -> Word64 -> [Word8] -> [Word8]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n 256) ((fromIntegral (mod n 256)):ans)

readTags:: Int -> ByteString -> (Int,(Word8,Word8))
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
    Compactible (Core.Value era)
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
    Compactible (Core.Value era)
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
instance (Crypto era ~ c, Era era) => HasField "compactAddress" (TxOut era) (CompactAddr c) where
  getField (TxOutCompact' a _) = a
  getField (TxOutCompactDH' a _ _) = a
  getField (TxOut a _ _) = compactAddr a

instance (Crypto era ~ c, Era era) => HasField "address" (TxOut era) (Addr c) where
  getField (TxOut a _ _) = a

instance (Core.Value era ~ val, Era era) => HasField "value" (TxOut era) val where
  getField (TxOut _ v _) = v

instance (c ~ Crypto era, Era era) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField (TxOut _ _ dhMay) = dhMay