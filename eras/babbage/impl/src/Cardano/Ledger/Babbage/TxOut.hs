{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxOut (
  BabbageTxOut (
    BabbageTxOut,
    TxOutCompact,
    TxOutCompact',
    TxOutCompactDH,
    TxOutCompactDH',
    TxOutCompactDatum,
    TxOutCompactRefScript,
    TxOut_AddrHash28_AdaOnly,
    TxOut_AddrHash28_AdaOnly_DataHash32
  ),
  BabbageEraTxOut (..),
  TxOut,
  addrEitherBabbageTxOutL,
  valueEitherBabbageTxOutL,
  dataHashBabbageTxOutL,
  dataBabbageTxOutL,
  datumBabbageTxOutL,
  referenceScriptBabbageTxOutL,
  getDatumBabbageTxOut,
  babbageMinUTxOValue,
  getEitherAddrBabbageTxOut,
  internBabbageTxOut,
) where

import Cardano.Ledger.Address (
  CompactAddr,
  compactAddr,
  decompactAddr,
  fromCborBothAddr,
 )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.TxBody (
  Addr28Extra,
  DataHash32,
  decodeAddress28,
  decodeDataHash32,
  encodeAddress28,
  encodeDataHash32,
  getAdaOnly,
 )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams (
  BabbageEraPParams (..),
  ppCoinsPerUTxOByteL,
 )
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.BaseTypes (
  KeyValuePairs (..),
  StrictMaybe (..),
  ToKeyValuePairs (..),
 )
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecShareCBOR (..),
  Decoder,
  DecoderError (..),
  EncCBOR (..),
  Encoding,
  FromCBOR (..),
  Interns,
  Sized (..),
  ToCBOR (..),
  TokenType (..),
  cborError,
  decodeBreakOr,
  decodeFullAnnotator,
  decodeListLenOrIndef,
  decodeMemPack,
  decodeNestedCborBytes,
  encodeListLen,
  encodeNestedCbor,
  getDecoderVersion,
  interns,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus.Data (
  BinaryData,
  Data,
  Datum (..),
  binaryDataToData,
  dataToBinaryData,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Aeson (ToJSON (..), (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.MemPack
import qualified Data.Text as T
import Data.Typeable (Proxy (..))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (NoThunks)

class (AlonzoEraTxOut era, AlonzoEraScript era) => BabbageEraTxOut era where
  referenceScriptTxOutL :: Lens' (TxOut era) (StrictMaybe (Script era))

  dataTxOutL :: Lens' (TxOut era) (StrictMaybe (Data era))

  datumTxOutL :: Lens' (TxOut era) (Datum era)

data BabbageTxOut era
  = TxOutCompact'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      !DataHash
  | TxOutCompactDatum
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !CompactAddr
      !(CompactForm (Value era))
      !(Datum era)
      !(Script era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential Staking)
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32
  deriving (Generic)

-- | This instance is backwards compatible in binary representation with TxOut instances for all
-- previous era
instance
  ( Era era
  , MemPack (Script era)
  , MemPack (CompactForm (Value era))
  ) =>
  MemPack (BabbageTxOut era)
  where
  packedByteCount = \case
    TxOutCompact' cAddr cValue ->
      packedTagByteCount + packedByteCount cAddr + packedByteCount cValue
    TxOutCompactDH' cAddr cValue dataHash ->
      packedTagByteCount + packedByteCount cAddr + packedByteCount cValue + packedByteCount dataHash
    TxOut_AddrHash28_AdaOnly cred addr28 cCoin ->
      packedTagByteCount + packedByteCount cred + packedByteCount addr28 + packedByteCount cCoin
    TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28 cCoin dataHash32 ->
      packedTagByteCount
        + packedByteCount cred
        + packedByteCount addr28
        + packedByteCount cCoin
        + packedByteCount dataHash32
    TxOutCompactDatum cAddr cValue datum ->
      packedTagByteCount + packedByteCount cAddr + packedByteCount cValue + packedByteCount datum
    TxOutCompactRefScript cAddr cValue datum script ->
      packedTagByteCount
        + packedByteCount cAddr
        + packedByteCount cValue
        + packedByteCount datum
        + packedByteCount script
  {-# INLINE packedByteCount #-}
  packM = \case
    TxOutCompact' cAddr cValue ->
      packTagM 0 >> packM cAddr >> packM cValue
    TxOutCompactDH' cAddr cValue dataHash ->
      packTagM 1 >> packM cAddr >> packM cValue >> packM dataHash
    TxOut_AddrHash28_AdaOnly cred addr28 cCoin ->
      packTagM 2 >> packM cred >> packM addr28 >> packM cCoin
    TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28 cCoin dataHash32 ->
      packTagM 3 >> packM cred >> packM addr28 >> packM cCoin >> packM dataHash32
    TxOutCompactDatum cAddr cValue datum ->
      packTagM 4 >> packM cAddr >> packM cValue >> packM datum
    TxOutCompactRefScript cAddr cValue datum script ->
      packTagM 5 >> packM cAddr >> packM cValue >> packM datum >> packM script
  {-# INLINE packM #-}
  unpackM =
    unpackM >>= \case
      0 -> TxOutCompact' <$> unpackM <*> unpackM
      1 -> TxOutCompactDH' <$> unpackM <*> unpackM <*> unpackM
      2 -> TxOut_AddrHash28_AdaOnly <$> unpackM <*> unpackM <*> unpackM
      3 -> TxOut_AddrHash28_AdaOnly_DataHash32 <$> unpackM <*> unpackM <*> unpackM <*> unpackM
      4 -> TxOutCompactDatum <$> unpackM <*> unpackM <*> unpackM
      5 -> TxOutCompactRefScript <$> unpackM <*> unpackM <*> unpackM <*> unpackM
      n -> unknownTagM @(BabbageTxOut era) n
  {-# INLINE unpackM #-}

instance EraTxOut BabbageEra where
  type TxOut BabbageEra = BabbageTxOut BabbageEra

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  upgradeTxOut = upgradeAlonzoTxOut

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

upgradeAlonzoTxOut :: Alonzo.AlonzoTxOut AlonzoEra -> BabbageTxOut BabbageEra
upgradeAlonzoTxOut = \case
  Alonzo.TxOutCompact' ca cv -> TxOutCompact' ca cv
  Alonzo.TxOutCompactDH' ca cv dh -> TxOutCompactDH' ca cv dh
  Alonzo.TxOut_AddrHash28_AdaOnly c a28e cc -> TxOut_AddrHash28_AdaOnly c a28e cc
  Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 c a28e cc dh32 -> TxOut_AddrHash28_AdaOnly_DataHash32 c a28e cc dh32

dataHashBabbageTxOutL ::
  EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe DataHash)
dataHashBabbageTxOutL =
  lens
    getDataHashBabbageTxOut
    ( \(BabbageTxOut addr cv _ s) -> \case
        SNothing -> BabbageTxOut addr cv NoDatum s
        SJust dh -> BabbageTxOut addr cv (DatumHash dh) s
    )
{-# INLINEABLE dataHashBabbageTxOutL #-}

instance AlonzoEraTxOut BabbageEra where
  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINEABLE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINEABLE datumTxOutF #-}

dataBabbageTxOutL :: EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe (Data era))
dataBabbageTxOutL =
  lens
    getDataBabbageTxOut
    ( \(BabbageTxOut addr cv _ s) ->
        \case
          SNothing -> BabbageTxOut addr cv NoDatum s
          SJust d -> BabbageTxOut addr cv (Datum (dataToBinaryData d)) s
    )
{-# INLINEABLE dataBabbageTxOutL #-}

datumBabbageTxOutL :: EraTxOut era => Lens' (BabbageTxOut era) (Datum era)
datumBabbageTxOutL =
  lens getDatumBabbageTxOut (\(BabbageTxOut addr cv _ s) d -> BabbageTxOut addr cv d s)
{-# INLINEABLE datumBabbageTxOutL #-}

referenceScriptBabbageTxOutL :: EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe (Script era))
referenceScriptBabbageTxOutL =
  lens getScriptBabbageTxOut (\(BabbageTxOut addr cv d _) s -> BabbageTxOut addr cv d s)
{-# INLINEABLE referenceScriptBabbageTxOutL #-}

instance BabbageEraTxOut BabbageEra where
  dataTxOutL = dataBabbageTxOutL
  {-# INLINEABLE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINEABLE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINEABLE referenceScriptTxOutL #-}

addrEitherBabbageTxOutL ::
  EraTxOut era =>
  Lens' (BabbageTxOut era) (Either Addr CompactAddr)
addrEitherBabbageTxOutL =
  lens
    getEitherAddrBabbageTxOut
    ( \txOut eAddr ->
        let cVal = getCompactValueBabbageTxOut txOut
            (_, _, datum, mScript) = viewTxOut txOut
         in case eAddr of
              Left addr -> mkTxOutCompact addr (compactAddr addr) cVal datum mScript
              Right cAddr -> mkTxOutCompact (decompactAddr cAddr) cAddr cVal datum mScript
    )
{-# INLINEABLE addrEitherBabbageTxOutL #-}

valueEitherBabbageTxOutL ::
  forall era.
  EraTxOut era =>
  Lens' (BabbageTxOut era) (Either (Value era) (CompactForm (Value era)))
valueEitherBabbageTxOutL =
  lens
    (Right . getCompactValueBabbageTxOut)
    ( \txOut eVal ->
        let (cAddr, _, datum, mScript) = viewCompactTxOut txOut
         in case eVal of
              Left val -> mkTxOut (decompactAddr cAddr) cAddr val datum mScript
              Right cVal -> mkTxOutCompact (decompactAddr cAddr) cAddr cVal datum mScript
    )
{-# INLINEABLE valueEitherBabbageTxOutL #-}

deriving stock instance
  (Era era, Eq (Script era), Eq (CompactForm (Value era))) =>
  Eq (BabbageTxOut era)

-- | Already in NF
instance NFData (BabbageTxOut era) where
  rnf = rwhnf

deriving via
  KeyValuePairs (BabbageTxOut era)
  instance
    (Era era, Val (Value era), ToJSON (Script era)) => ToJSON (BabbageTxOut era)

instance (Era era, Val (Value era), ToJSON (Script era)) => ToKeyValuePairs (BabbageTxOut era) where
  toKeyValuePairs (BabbageTxOut !addr !val !dat !mRefScript) =
    [ "address" .= addr
    , "value" .= val
    , "datum" .= dat
    , "referenceScript" .= mRefScript
    ]

viewCompactTxOut ::
  forall era.
  Val (Value era) =>
  BabbageTxOut era ->
  (CompactAddr, CompactForm (Value era), Datum era, StrictMaybe (Script era))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, NoDatum, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, DatumHash dh, SNothing)
  TxOutCompactDatum addr val datum -> (addr, val, Datum datum, SNothing)
  TxOutCompactRefScript addr val datum rs -> (addr, val, datum, SJust rs)
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal ->
    let (a, b, c) =
          Alonzo.viewCompactTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
     in (a, b, toDatum c, SNothing)
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32 ->
    let (a, b, c) =
          Alonzo.viewCompactTxOut @era $
            Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32
     in (a, b, toDatum c, SNothing)
  where
    toDatum = \case
      SNothing -> NoDatum
      SJust dh -> DatumHash dh
{-# INLINEABLE viewCompactTxOut #-}

viewTxOut ::
  forall era.
  Val (Value era) =>
  BabbageTxOut era ->
  (Addr, Value era, Datum era, StrictMaybe (Script era))
viewTxOut (TxOutCompact' bs c) = (addr, val, NoDatum, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh) = (addr, val, DatumHash dh, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDatum bs c d) = (addr, val, Datum d, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactRefScript bs c d rs) = (addr, val, d, SJust rs)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal) = (addr, val, NoDatum, SNothing)
  where
    (addr, val, _) =
      Alonzo.viewTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32) =
  case mDataHash of
    SNothing -> (addr, val, NoDatum, SNothing)
    SJust dh -> (addr, val, DatumHash dh, SNothing)
  where
    (addr, val, mDataHash) =
      Alonzo.viewTxOut @era $
        Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32
{-# INLINEABLE viewTxOut #-}

instance
  (Era era, Show (Script era), Val (Value era)) =>
  Show (BabbageTxOut era)
  where
  show = show . viewTxOut

instance (Era era, NoThunks (Script era), Val (Value era)) => NoThunks (BabbageTxOut era)

pattern BabbageTxOut ::
  (Era era, Val (Value era), HasCallStack) =>
  Addr ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
pattern BabbageTxOut addr vl datum refScript <-
  (viewTxOut -> (addr, vl, datum, refScript))
  where
    BabbageTxOut addr vl datum refScript = mkTxOut addr (compactAddr addr) vl datum refScript

{-# COMPLETE BabbageTxOut #-}

-- | Helper function for constructing a BabbageTxOut. Both compacted and uncompacted
-- address should be the exact same address in different forms.
mkTxOut ::
  forall era.
  (Val (Value era), HasCallStack) =>
  Addr ->
  CompactAddr ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
mkTxOut addr _cAddr vl NoDatum SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl
  , Addr network paymentCred stakeRef <- addr
  , StakeRefBase stakeCred <- stakeRef =
      let
        addr28Extra = encodeAddress28 network paymentCred
       in
        TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
mkTxOut addr _cAddr vl (DatumHash dh) SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl
  , Addr network paymentCred stakeRef <- addr
  , StakeRefBase stakeCred <- stakeRef =
      let
        addr28Extra = encodeAddress28 network paymentCred
        dataHash32 = encodeDataHash32 dh
       in
        TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
mkTxOut _addr cAddr vl d rs =
  let cVal = fromMaybe (error ("Illegal Value in TxOut: " ++ show vl)) $ toCompact vl
   in case rs of
        SNothing -> case d of
          NoDatum -> TxOutCompact' cAddr cVal
          DatumHash dh -> TxOutCompactDH' cAddr cVal dh
          Datum binaryData -> TxOutCompactDatum cAddr cVal binaryData
        SJust rs' -> TxOutCompactRefScript cAddr cVal d rs'
{-# INLINEABLE mkTxOut #-}

-- TODO: Implement mkTxOut in terms of mkTxOutCompact, it will avoid unnecessary
-- MultiAsset serilization/deserialization
mkTxOutCompact ::
  Val (Value era) =>
  Addr ->
  CompactAddr ->
  CompactForm (Value era) ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
mkTxOutCompact addr cAddr cVal = mkTxOut addr cAddr (fromCompact cVal)
{-# INLINE mkTxOutCompact #-}

pattern TxOutCompact ::
  (Era era, Val (Value era), Compactible (Value era), HasCallStack) =>
  CompactAddr ->
  CompactForm (Value era) ->
  BabbageTxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, NoDatum, SNothing))
  where
    TxOutCompact cAddr cVal
      | isAdaOnlyCompact cVal =
          mkTxOut (decompactAddr cAddr) cAddr (fromCompact cVal) NoDatum SNothing
      | otherwise = TxOutCompact' cAddr cVal

pattern TxOutCompactDH ::
  (Era era, Val (Value era), Compactible (Value era), HasCallStack) =>
  CompactAddr ->
  CompactForm (Value era) ->
  DataHash ->
  BabbageTxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, DatumHash dh, SNothing))
  where
    TxOutCompactDH cAddr cVal dh
      | isAdaOnlyCompact cVal =
          mkTxOut (decompactAddr cAddr) cAddr (fromCompact cVal) (DatumHash dh) SNothing
      | otherwise = TxOutCompactDH' cAddr cVal dh

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

instance (EraScript era, Val (Value era)) => ToCBOR (BabbageTxOut era) where
  toCBOR = toEraCBOR @era
  {-# INLINE toCBOR #-}

instance (EraScript era, Val (Value era)) => FromCBOR (BabbageTxOut era) where
  fromCBOR = fromEraCBOR @era
  {-# INLINE fromCBOR #-}

instance (EraScript era, Val (Value era)) => EncCBOR (BabbageTxOut era) where
  encCBOR = \case
    TxOutCompactRefScript addr cv d rs -> encodeTxOut addr cv d (SJust rs)
    TxOutCompactDatum addr cv d -> encodeTxOut addr cv (Datum d) SNothing
    TxOutCompactDH addr cv dh -> encodeListLen 3 <> encCBOR addr <> encCBOR cv <> encCBOR dh
    TxOutCompact addr cv -> encodeListLen 2 <> encCBOR addr <> encCBOR cv

instance (EraScript era, Val (Value era)) => DecCBOR (BabbageTxOut era) where
  decCBOR = decodeBabbageTxOut fromCborBothAddr
  {-# INLINE decCBOR #-}

instance
  ( EraScript era
  , Val (Value era)
  , MemPack (Script era)
  , MemPack (CompactForm (Value era))
  ) =>
  DecShareCBOR (BabbageTxOut era)
  where
  type Share (BabbageTxOut era) = Interns (Credential Staking)
  decShareCBOR credsInterns = do
    txOut <-
      peekTokenType >>= \case
        TypeBytes -> decodeMemPack
        TypeBytesIndef -> decodeMemPack
        _ -> decCBOR
    pure $! internBabbageTxOut (interns credsInterns) txOut
  {-# INLINEABLE decShareCBOR #-}

internBabbageTxOut ::
  (Credential Staking -> Credential Staking) ->
  BabbageTxOut era ->
  BabbageTxOut era
internBabbageTxOut internCred = \case
  TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
    TxOut_AddrHash28_AdaOnly (internCred cred) addr28Extra ada
  TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
    TxOut_AddrHash28_AdaOnly_DataHash32 (internCred cred) addr28Extra ada dataHash32
  txOut -> txOut
{-# INLINE internBabbageTxOut #-}

decodeBabbageTxOut ::
  (EraScript era, Val (Value era)) =>
  -- | We need to use a backwards compatible decoder for any address in a pre-babbage
  -- TxOut format. This is needed in order to get rid of bogus pointers from the ledger
  -- state in Conway
  (forall s'. Decoder s' (Addr, CompactAddr)) ->
  Decoder s (BabbageTxOut era)
decodeBabbageTxOut decAddr = do
  peekTokenType >>= \case
    TypeMapLenIndef -> decodeTxOut decAddr
    TypeMapLen -> decodeTxOut decAddr
    _ -> oldTxOut
  where
    oldTxOut = do
      lenOrIndef <- decodeListLenOrIndef
      case lenOrIndef of
        Nothing -> do
          (a, ca) <- decAddr
          v <- decCBOR
          decodeBreakOr >>= \case
            True -> pure $ mkTxOut a ca v NoDatum SNothing
            False -> do
              dh <- decCBOR
              decodeBreakOr >>= \case
                True -> pure $ mkTxOut a ca v (DatumHash dh) SNothing
                False -> cborError $ DecoderErrorCustom "TxOut" "Excess terms in TxOut"
        Just 2 -> do
          (a, ca) <- decAddr
          v <- decCBOR
          pure $ mkTxOut a ca v NoDatum SNothing
        Just 3 -> do
          (a, ca) <- decAddr
          v <- decCBOR
          dh <- decCBOR
          pure $ mkTxOut a ca v (DatumHash dh) SNothing
        Just _ -> cborError $ DecoderErrorCustom "TxOut" "Wrong number of terms in TxOut"
    {-# INLINE oldTxOut #-}
{-# INLINEABLE decodeBabbageTxOut #-}

encodeTxOut ::
  forall era.
  (EraScript era, Val (Value era)) =>
  CompactAddr ->
  CompactForm (Value era) ->
  Datum era ->
  StrictMaybe (Script era) ->
  Encoding
encodeTxOut cAddr cVal datum script =
  encode $
    Keyed (,,,,)
      !> Key 0 (To cAddr)
      !> Key 1 (To (fromCompact cVal))
      !> Omit (== NoDatum) (Key 2 (To datum))
      !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script
{-# INLINE encodeTxOut #-}

data DecodingTxOut era = DecodingTxOut
  { decodingTxOutAddr :: !(StrictMaybe (Addr, CompactAddr))
  , decodingTxOutVal :: !(Value era)
  , decodingTxOutDatum :: !(Datum era)
  , decodingTxOutScript :: !(StrictMaybe (Script era))
  }

decodeTxOut ::
  forall s era.
  (EraScript era, Val (Value era)) =>
  (forall s'. Decoder s' (Addr, CompactAddr)) ->
  Decoder s (BabbageTxOut era)
decodeTxOut decAddr = do
  dtxo <- decode $ SparseKeyed "TxOut" initial bodyFields requiredFields
  case dtxo of
    DecodingTxOut SNothing _ _ _ ->
      cborError $ DecoderErrorCustom "BabbageTxOut" "Impossible: no Addr"
    DecodingTxOut (SJust (addr, cAddr)) val d script ->
      pure $ mkTxOut addr cAddr val d script
  where
    initial :: DecodingTxOut era
    initial = DecodingTxOut SNothing mempty NoDatum SNothing
    bodyFields :: (Word -> Field (DecodingTxOut era))
    bodyFields 0 =
      field
        (\x txo -> txo {decodingTxOutAddr = SJust x})
        (D decAddr)
    bodyFields 1 =
      field
        (\x txo -> txo {decodingTxOutVal = x})
        From
    bodyFields 2 =
      field
        (\x txo -> txo {decodingTxOutDatum = x})
        (D decCBOR)
    bodyFields 3 =
      ofield
        (\x txo -> txo {decodingTxOutScript = x})
        (D $ decodeCIC "Script")
    bodyFields n = invalidField n
    {-# INLINE bodyFields #-}
    requiredFields =
      [ (0, "addr")
      , (1, "val")
      ]
{-# INLINE decodeTxOut #-}

babbageMinUTxOValue ::
  BabbageEraPParams era =>
  PParams era ->
  Sized a ->
  Coin
babbageMinUTxOValue pp sizedTxOut =
  Coin $ fromIntegral (constantOverhead + sizedSize sizedTxOut) * fromIntegral cpb
  where
    CoinPerByte (CompactCoin cpb) = pp ^. ppCoinsPerUTxOByteL
    -- This constant is an approximation of the memory overhead that comes
    -- from TxIn and an entry in the Map data structure:
    --
    -- 160 = 20 words * 8bytes
    --
    -- This means that if:
    --
    --  * 'coinsPerUTxOByte' = 4310
    --  * A simple TxOut with staking and payment credentials with ADA only
    --    amount of 978370 lovelace
    --
    -- we get the size of TxOut to be 67 bytes and the minimum value will come
    -- out to be 978597 lovelace. Also the absolute minimum value will be
    -- 857690, because TxOut without staking address can't be less than 39 bytes
    constantOverhead = 160
{-# INLINE babbageMinUTxOValue #-}

getEitherAddrBabbageTxOut ::
  BabbageTxOut era ->
  Either Addr CompactAddr
getEitherAddrBabbageTxOut = \case
  TxOutCompact' cAddr _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ -> Right cAddr
  TxOutCompactRefScript cAddr _ _ _ -> Right cAddr
  TxOutCompactDatum cAddr _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _ ->
    Left $ decodeAddress28 stakeRef addr28Extra
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _ ->
    Left $ decodeAddress28 stakeRef addr28Extra
{-# INLINE getEitherAddrBabbageTxOut #-}

-- TODO: Switch to using `getDatumBabbageTxOut`
getDataBabbageTxOut :: Era era => BabbageTxOut era -> StrictMaybe (Data era)
getDataBabbageTxOut = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' {} -> SNothing
  TxOutCompactDatum _ _ binaryData -> SJust $ binaryDataToData binaryData
  TxOutCompactRefScript _ _ datum _
    | Datum binaryData <- datum -> SJust $ binaryDataToData binaryData
    | otherwise -> SNothing
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> SNothing
{-# INLINE getDataBabbageTxOut #-}

-- TODO: Switch to using `getDatumBabbageTxOut`

-- | Return the data hash of a given transaction output, if one is present.
--  Note that this function does *not* return the hash of an inline datum
--  if one is present.
getDataHashBabbageTxOut ::
  BabbageTxOut era ->
  StrictMaybe DataHash
getDataHashBabbageTxOut txOut =
  case getDatumBabbageTxOut txOut of
    NoDatum -> SNothing
    DatumHash dh -> SJust dh
    Datum _d -> SNothing
{-# INLINE getDataHashBabbageTxOut #-}

getScriptBabbageTxOut :: BabbageTxOut era -> StrictMaybe (Script era)
getScriptBabbageTxOut = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' {} -> SNothing
  TxOutCompactDatum {} -> SNothing
  TxOutCompactRefScript _ _ _ s -> SJust s
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> SNothing
{-# INLINE getScriptBabbageTxOut #-}

getDatumBabbageTxOut :: BabbageTxOut era -> Datum era
getDatumBabbageTxOut = \case
  TxOutCompact' {} -> NoDatum
  TxOutCompactDH' _ _ dh -> DatumHash dh
  TxOutCompactDatum _ _ binaryData -> Datum binaryData
  TxOutCompactRefScript _ _ datum _ -> datum
  TxOut_AddrHash28_AdaOnly {} -> NoDatum
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32 ->
    DatumHash $ decodeDataHash32 dataHash32
{-# INLINEABLE getDatumBabbageTxOut #-}

getCompactValueBabbageTxOut :: EraTxOut era => BabbageTxOut era -> CompactForm (Value era)
getCompactValueBabbageTxOut =
  \case
    TxOutCompact' _ cv -> cv
    TxOutCompactDH' _ cv _ -> cv
    TxOutCompactDatum _ cv _ -> cv
    TxOutCompactRefScript _ cv _ _ -> cv
    TxOut_AddrHash28_AdaOnly _ _ cc -> injectCompact cc
    TxOut_AddrHash28_AdaOnly_DataHash32 _ _ cc _ -> injectCompact cc
{-# INLINE getCompactValueBabbageTxOut #-}

decodeCIC :: DecCBOR (Annotator b) => T.Text -> Decoder s b
decodeCIC s = do
  version <- getDecoderVersion
  lbs <- decodeNestedCborBytes
  case decodeFullAnnotator version s decCBOR (LBS.fromStrict lbs) of
    Left e -> fail $ T.unpack s <> ": " <> show e
    Right x -> pure x
{-# INLINEABLE decodeCIC #-}
