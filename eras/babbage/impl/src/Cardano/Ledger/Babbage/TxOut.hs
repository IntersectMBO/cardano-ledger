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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxOut (
  BabbageTxOut (
    BabbageTxOut,
    TxOutCompact,
    TxOutCompactDH,
    TxOutCompactDatum,
    TxOutCompactRefScript
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
  txOutData,
  txOutDataHash,
  txOutScript,
) where

import Cardano.Crypto.Hash (HashAlgorithm)
import Cardano.Ledger.Address (
  Addr (..),
  CompactAddr,
  compactAddr,
  decompactAddr,
  fromCborBackwardsBothAddr,
  fromCborBothAddr,
 )
import Cardano.Ledger.Alonzo.Scripts.Data (
  BinaryData,
  Data,
  Datum (..),
  binaryDataToData,
  dataToBinaryData,
 )
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
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams ()
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.BaseTypes (
  StrictMaybe (..),
  maybeToStrictMaybe,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (
  Annotator (..),
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
  decodeNestedCborBytes,
  encodeNestedCbor,
  getDecoderVersion,
  interns,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (ADDRHASH), StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad ((<$!>))
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), (:~:) (Refl))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (NoThunks)
import Prelude hiding (lookup)

data BabbageTxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (EraCrypto era))
      !(CompactForm (Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (EraCrypto era))
      !(CompactForm (Value era))
      !(DataHash (EraCrypto era))
  | TxOutCompactDatum
      {-# UNPACK #-} !(CompactAddr (EraCrypto era))
      !(CompactForm (Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !(CompactAddr (EraCrypto era))
      !(CompactForm (Value era))
      !(Datum era)
      !(Script era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (EraCrypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (EraCrypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32
  deriving (Generic)

instance Crypto c => EraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance EraTxOut (BabbageEra StandardCrypto) #-}

  type TxOut (BabbageEra c) = BabbageTxOut (BabbageEra c)

  mkBasicTxOut addr vl = BabbageTxOut addr vl NoDatum SNothing

  addrEitherTxOutL = addrEitherBabbageTxOutL
  {-# INLINE addrEitherTxOutL #-}

  valueEitherTxOutL = valueEitherBabbageTxOutL
  {-# INLINE valueEitherTxOutL #-}

  getMinCoinSizedTxOut = babbageMinUTxOValue

dataHashBabbageTxOutL ::
  EraTxOut era => Lens' (BabbageTxOut era) (StrictMaybe (DataHash (EraCrypto era)))
dataHashBabbageTxOutL =
  lens
    getDataHashBabbageTxOut
    ( \(BabbageTxOut addr cv _ s) -> \case
        SNothing -> BabbageTxOut addr cv NoDatum s
        SJust dh -> BabbageTxOut addr cv (DatumHash dh) s
    )
{-# INLINEABLE dataHashBabbageTxOutL #-}

instance Crypto c => AlonzoEraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (BabbageEra StandardCrypto) #-}

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

instance Crypto c => BabbageEraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance BabbageEraTxOut (BabbageEra StandardCrypto) #-}
  dataTxOutL = dataBabbageTxOutL
  {-# INLINEABLE dataTxOutL #-}

  datumTxOutL = datumBabbageTxOutL
  {-# INLINEABLE datumTxOutL #-}

  referenceScriptTxOutL = referenceScriptBabbageTxOutL
  {-# INLINEABLE referenceScriptTxOutL #-}

addrEitherBabbageTxOutL ::
  EraTxOut era =>
  Lens' (BabbageTxOut era) (Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era)))
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

instance
  (Era era, ToJSON (Datum era), ToJSON (Script era), Val (Value era)) =>
  ToJSON (BabbageTxOut era)
  where
  toJSON = object . toBabbageTxOutPairs
  toEncoding = pairs . mconcat . toBabbageTxOutPairs

toBabbageTxOutPairs ::
  ( Era era
  , KeyValue a
  , Val (Value era)
  , ToJSON (Script era)
  ) =>
  BabbageTxOut era ->
  [a]
toBabbageTxOutPairs (BabbageTxOut !addr !val !dat !mRefScript) =
  [ "address" .= addr
  , "value" .= val
  , "datum" .= dat
  , "referenceScript" .= mRefScript
  ]

viewCompactTxOut ::
  forall era.
  (Era era, Val (Value era)) =>
  BabbageTxOut era ->
  (CompactAddr (EraCrypto era), CompactForm (Value era), Datum era, StrictMaybe (Script era))
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
  (Era era, Val (Value era)) =>
  BabbageTxOut era ->
  (Addr (EraCrypto era), Value era, Datum era, StrictMaybe (Script era))
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
  Addr (EraCrypto era) ->
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
  (Era era, Val (Value era), HasCallStack) =>
  Addr (EraCrypto era) ->
  CompactAddr (EraCrypto era) ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
mkTxOut addr _cAddr vl NoDatum SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl
  , Addr network paymentCred stakeRef <- addr
  , StakeRefBase stakeCred <- stakeRef
  , Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
      TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
mkTxOut addr _cAddr vl (DatumHash dh) SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl
  , Addr network paymentCred stakeRef <- addr
  , StakeRefBase stakeCred <- stakeRef
  , Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred
  , Just (Refl, dataHash32) <- encodeDataHash32 dh =
      TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
mkTxOut _addr cAddr vl d rs =
  let cVal = fromMaybe (error "Illegal value in txout") $ toCompact vl
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
  (Era era, Val (Value era)) =>
  Addr (EraCrypto era) ->
  CompactAddr (EraCrypto era) ->
  CompactForm (Value era) ->
  Datum era ->
  StrictMaybe (Script era) ->
  BabbageTxOut era
mkTxOutCompact addr cAddr cVal = mkTxOut addr cAddr (fromCompact cVal)
{-# INLINE mkTxOutCompact #-}

pattern TxOutCompact ::
  (Era era, Val (Value era), Compactible (Value era), HasCallStack) =>
  CompactAddr (EraCrypto era) ->
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
  CompactAddr (EraCrypto era) ->
  CompactForm (Value era) ->
  DataHash (EraCrypto era) ->
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
    TxOutCompactDH addr cv dh -> encodeTxOut @era addr cv (DatumHash dh) SNothing
    TxOutCompact addr cv -> encodeTxOut @era addr cv NoDatum SNothing

instance (EraScript era, Val (Value era)) => DecCBOR (BabbageTxOut era) where
  decCBOR = decodeBabbageTxOut fromCborBothAddr
  {-# INLINE decCBOR #-}

instance (EraScript era, Val (Value era)) => DecShareCBOR (BabbageTxOut era) where
  type Share (BabbageTxOut era) = Interns (Credential 'Staking (EraCrypto era))
  decShareCBOR credsInterns =
    -- Even in Babbage the ledger state still contains garbage pointers that we need to
    -- deal with. This will be taken care of upon entry to Conway era. After which this
    -- backwards compatibility shim can be removed.
    internTxOut <$!> decodeBabbageTxOut fromCborBackwardsBothAddr
    where
      internTxOut = \case
        TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
          TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada
        TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
          TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32
        txOut -> txOut
  {-# INLINEABLE decShareCBOR #-}

decodeBabbageTxOut ::
  (EraScript era, Val (Value era)) =>
  -- | We need to use a backwards compatible decoder for any address in a pre-babbage
  -- TxOut format. This is needed in order to get rid of bogus pointers from the ledger
  -- state in Conway
  (forall s'. Decoder s' (Addr (EraCrypto era), CompactAddr (EraCrypto era))) ->
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
  CompactAddr (EraCrypto era) ->
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
  { decodingTxOutAddr :: !(StrictMaybe (Addr (EraCrypto era), CompactAddr (EraCrypto era)))
  , decodingTxOutVal :: !(Value era)
  , decodingTxOutDatum :: !(Datum era)
  , decodingTxOutScript :: !(StrictMaybe (Script era))
  }

decodeTxOut ::
  forall s era.
  (EraScript era, Val (Value era)) =>
  (forall s'. Decoder s' (Addr (EraCrypto era), CompactAddr (EraCrypto era))) ->
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
    bodyFields n = field (\_ t -> t) (Invalid n)
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
  Coin $ fromIntegral (constantOverhead + sizedSize sizedTxOut) * cpb
  where
    CoinPerByte (Coin cpb) = pp ^. ppCoinsPerUTxOByteL
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
  HashAlgorithm (ADDRHASH (EraCrypto era)) =>
  BabbageTxOut era ->
  Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era))
getEitherAddrBabbageTxOut = \case
  TxOutCompact' cAddr _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ -> Right cAddr
  TxOutCompactRefScript cAddr _ _ _ -> Right cAddr
  TxOutCompactDatum cAddr _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address of non-standard size"
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address or a hash of non-standard size"
{-# INLINEABLE getEitherAddrBabbageTxOut #-}

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
getDataHashBabbageTxOut :: Era era => BabbageTxOut era -> StrictMaybe (DataHash (EraCrypto era))
getDataHashBabbageTxOut = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' _ _ dh -> SJust dh
  TxOutCompactDatum {} -> SNothing
  TxOutCompactRefScript _ _ datum _ ->
    case datum of
      NoDatum -> SNothing
      DatumHash dh -> SJust dh
      Datum _d -> SNothing
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32 ->
    maybeToStrictMaybe $ decodeDataHash32 dataHash32
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

getDatumBabbageTxOut :: Era era => BabbageTxOut era -> Datum era
getDatumBabbageTxOut = \case
  TxOutCompact' {} -> NoDatum
  TxOutCompactDH' _ _ dh -> DatumHash dh
  TxOutCompactDatum _ _ binaryData -> Datum binaryData
  TxOutCompactRefScript _ _ datum _ -> datum
  TxOut_AddrHash28_AdaOnly {} -> NoDatum
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32
    | Just dh <- decodeDataHash32 dataHash32 -> DatumHash dh
    | otherwise -> error "Impossible: Compacted a hash of non-standard size"
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

txOutData :: Era era => BabbageTxOut era -> Maybe (Data era)
txOutData = strictMaybeToMaybe . getDataBabbageTxOut
{-# DEPRECATED txOutData "In favor of `dataTxOutL` or `getDataBabbageTxOut`" #-}

txOutDataHash :: Era era => BabbageTxOut era -> Maybe (DataHash (EraCrypto era))
txOutDataHash = strictMaybeToMaybe . getDataHashBabbageTxOut
{-# DEPRECATED txOutDataHash "In favor of `dataHashTxOutL` or `getDataHashBabbageTxOut`" #-}

txOutScript :: BabbageTxOut era -> Maybe (Script era)
txOutScript = strictMaybeToMaybe . getScriptBabbageTxOut
{-# DEPRECATED txOutScript "In favor of `dataTxOutL` or `getScriptBabbageTxOut`" #-}

decodeCIC :: DecCBOR (Annotator b) => T.Text -> Decoder s b
decodeCIC s = do
  version <- getDecoderVersion
  lbs <- decodeNestedCborBytes
  case decodeFullAnnotator version s decCBOR (LBS.fromStrict lbs) of
    Left e -> fail $ T.unpack s <> ": " <> show e
    Right x -> pure x
{-# INLINEABLE decodeCIC #-}
