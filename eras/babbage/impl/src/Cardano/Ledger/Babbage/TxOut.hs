{-# LANGUAGE DataKinds #-}
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

module Cardano.Ledger.Babbage.TxOut
  ( BabbageTxOut
      ( BabbageTxOut,
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
    fromCborTxOutWithAddr,
  )
where

import Cardano.Crypto.Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data
  ( BinaryData,
    Data,
    Datum (..),
    binaryDataToData,
    dataToBinaryData,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( Addr28Extra,
    AlonzoEraTxOut (..),
    DataHash32,
    decodeAddress28,
    decodeDataHash32,
    encodeAddress28,
    encodeDataHash32,
    getAdaOnly,
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.PParams (_coinsPerUTxOByte)
import Cardano.Ledger.Babbage.Scripts ()
import Cardano.Ledger.BaseTypes
  ( StrictMaybe (..),
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Cardano.Ledger.Binary
  ( Annotator (..),
    Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    FromSharedCBOR (..),
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
import Cardano.Ledger.CompactAddress
  ( CompactAddr,
    compactAddr,
    decompactAddr,
    fromCborBackwardsBothAddr,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core (TxOut)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative (decodeNonNegative),
    Val (..),
  )
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), (:~:) (Refl))
import Data.Word
import GHC.Records
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
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

type TxOut era = BabbageTxOut era

{-# DEPRECATED TxOut "Use `BabbageTxOut` instead" #-}

instance CC.Crypto c => EraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance EraTxOut (BabbageEra CC.StandardCrypto) #-}

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

instance CC.Crypto c => AlonzoEraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxOut (BabbageEra CC.StandardCrypto) #-}

  dataHashTxOutL = dataHashBabbageTxOutL
  {-# INLINEABLE dataHashTxOutL #-}

  datumTxOutF = to getDatumBabbageTxOut
  {-# INLINEABLE datumTxOutF #-}

class (AlonzoEraTxOut era, EraScript era) => BabbageEraTxOut era where
  referenceScriptTxOutL :: Lens' (Core.TxOut era) (StrictMaybe (Script era))

  dataTxOutL :: Lens' (Core.TxOut era) (StrictMaybe (Data era))

  datumTxOutL :: Lens' (Core.TxOut era) (Datum era)

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

instance CC.Crypto c => BabbageEraTxOut (BabbageEra c) where
  {-# SPECIALIZE instance BabbageEraTxOut (BabbageEra CC.StandardCrypto) #-}
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

viewCompactTxOut ::
  forall era.
  EraTxOut era =>
  TxOut era ->
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

viewTxOut ::
  forall era.
  (Era era, Val (Value era)) =>
  TxOut era ->
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

instance
  (Era era, Show (Value era), Show (Script era), Val (Value era)) =>
  Show (BabbageTxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "BabbageTxOut" (BabbageTxOut era) instance NoThunks (BabbageTxOut era)

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
  | Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Addr network paymentCred stakeRef <- addr,
    StakeRefBase stakeCred <- stakeRef,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
      TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
mkTxOut addr _cAddr vl (DatumHash dh) SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Addr network paymentCred stakeRef <- addr,
    StakeRefBase stakeCred <- stakeRef,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred,
    Just (Refl, dataHash32) <- encodeDataHash32 dh =
      TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
mkTxOut _addr cAddr vl d rs =
  let cVal = fromMaybe (error "Illegal value in txout") $ toCompact vl
   in case rs of
        SNothing -> case d of
          NoDatum -> TxOutCompact' cAddr cVal
          DatumHash dh -> TxOutCompactDH' cAddr cVal dh
          Datum binaryData -> TxOutCompactDatum cAddr cVal binaryData
        SJust rs' -> TxOutCompactRefScript cAddr cVal d rs'

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

pattern TxOutCompact ::
  (EraTxOut era, HasCallStack) =>
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
  (EraTxOut era, HasCallStack) =>
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

instance
  (Era era, Val (Value era), ToCBOR (Value era), ToCBOR (Script era)) =>
  ToCBOR (BabbageTxOut era)
  where
  toCBOR (BabbageTxOut addr v d s) = encodeTxOut (compactAddr addr) v d s

-- FIXME: ^ Starting with Babbage we need to reserialize all Addresses.  It is
-- safe to reserialize an address, because we do not rely on this instance for
-- computing a hash of a transaction and it is only used in storing TxOuts in
-- the ledger state.
--
-- After Vasil Hardfork we can switch it back to a more efficient version below:
--
-- toCBOR (TxOutCompact addr cv) = encodeTxOut @era addr cv NoDatum SNothing
-- toCBOR (TxOutCompactDH addr cv dh) = encodeTxOut @era addr cv (DatumHash dh) SNothing
-- toCBOR (TxOutCompactDatum addr cv d) = encodeTxOut addr cv (Datum d) SNothing
-- toCBOR (TxOutCompactRefScript addr cv d rs) = encodeTxOut addr cv d (SJust rs)

instance
  ( Era era,
    Val (Value era),
    FromCBOR (Annotator (Script era)),
    DecodeNonNegative (Value era)
  ) =>
  FromCBOR (BabbageTxOut era)
  where
  fromCBOR = fromCborTxOutWithAddr fromCborBackwardsBothAddr

instance
  ( Era era,
    Val (Value era),
    FromCBOR (Annotator (Script era)),
    DecodeNonNegative (Value era)
  ) =>
  FromSharedCBOR (BabbageTxOut era)
  where
  type Share (BabbageTxOut era) = Interns (Credential 'Staking (EraCrypto era))
  fromSharedCBOR credsInterns =
    internTxOut <$!> fromCborTxOutWithAddr fromCborBackwardsBothAddr
    where
      internTxOut = \case
        TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
          TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada
        TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
          TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32
        txOut -> txOut

fromCborTxOutWithAddr ::
  (Era era, Val (Value era), FromCBOR (Annotator (Script era)), DecodeNonNegative (Value era)) =>
  (forall s'. Decoder s' (Addr (EraCrypto era), CompactAddr (EraCrypto era))) ->
  Decoder s (BabbageTxOut era)
fromCborTxOutWithAddr decAddr = do
  peekTokenType >>= \case
    TypeMapLenIndef -> decodeTxOut decAddr
    TypeMapLen -> decodeTxOut decAddr
    _ -> oldTxOut
  where
    oldTxOut = do
      lenOrIndef <- decodeListLenOrIndef
      case lenOrIndef of
        Nothing -> do
          (a, ca) <- fromCborBackwardsBothAddr
          v <- decodeNonNegative
          decodeBreakOr >>= \case
            True -> pure $ mkTxOut a ca v NoDatum SNothing
            False -> do
              dh <- fromCBOR
              decodeBreakOr >>= \case
                True -> pure $ mkTxOut a ca v (DatumHash dh) SNothing
                False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
        Just 2 -> do
          (a, ca) <- decAddr
          v <- decodeNonNegative
          pure $ mkTxOut a ca v NoDatum SNothing
        Just 3 -> do
          (a, ca) <- fromCborBackwardsBothAddr
          v <- decodeNonNegative
          dh <- fromCBOR
          pure $ mkTxOut a ca v (DatumHash dh) SNothing
        Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"

{-# INLINE encodeTxOut #-}
encodeTxOut ::
  forall era.
  (Era era, ToCBOR (Value era), ToCBOR (Script era)) =>
  CompactAddr (EraCrypto era) ->
  Value era ->
  Datum era ->
  StrictMaybe (Script era) ->
  Encoding
encodeTxOut addr val datum script =
  encode $
    Keyed (,,,,)
      !> Key 0 (To addr)
      !> Key 1 (To val)
      !> Omit (== NoDatum) (Key 2 (To datum))
      !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script

data DecodingTxOut era = DecodingTxOut
  { decodingTxOutAddr :: !(StrictMaybe (Addr (EraCrypto era), CompactAddr (EraCrypto era))),
    decodingTxOutVal :: !(Value era),
    decodingTxOutDatum :: !(Datum era),
    decodingTxOutScript :: !(StrictMaybe (Script era))
  }

{-# INLINE decodeTxOut #-}
decodeTxOut ::
  forall s era.
  (Era era, Val (Value era), DecodeNonNegative (Value era), FromCBOR (Annotator (Script era))) =>
  (forall s'. Decoder s' (Addr (EraCrypto era), CompactAddr (EraCrypto era))) ->
  Decoder s (BabbageTxOut era)
decodeTxOut decAddr = do
  dtxo <- decode $ SparseKeyed "TxOut" initial bodyFields requiredFields
  case dtxo of
    DecodingTxOut SNothing _ _ _ -> cborError $ DecoderErrorCustom "BabbageTxOut" "Impossible: no Addr"
    DecodingTxOut (SJust (addr, cAddr)) val d script -> pure $ mkTxOut addr cAddr val d script
  where
    initial :: DecodingTxOut era
    initial =
      DecodingTxOut SNothing mempty NoDatum SNothing
    bodyFields :: (Word -> Field (DecodingTxOut era))
    bodyFields 0 =
      field
        (\x txo -> txo {decodingTxOutAddr = SJust x})
        (D decAddr)
    bodyFields 1 =
      field
        (\x txo -> txo {decodingTxOutVal = x})
        (D decodeNonNegative)
    bodyFields 2 =
      field
        (\x txo -> txo {decodingTxOutDatum = x})
        (D fromCBOR)
    bodyFields 3 =
      ofield
        (\x txo -> txo {decodingTxOutScript = x})
        (D $ decodeCIC "Script")
    bodyFields n = field (\_ t -> t) (Invalid n)
    requiredFields =
      [ (0, "addr"),
        (1, "val")
      ]

babbageMinUTxOValue ::
  HasField "_coinsPerUTxOByte" (PParams era) Coin =>
  PParams era ->
  Sized a ->
  Coin
babbageMinUTxOValue pp sizedTxOut =
  Coin $ fromIntegral (constantOverhead + sizedSize sizedTxOut) * unCoin coinsPerUTxOByte
  where
    coinsPerUTxOByte = getField @"_coinsPerUTxOByte" pp
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

getEitherAddrBabbageTxOut ::
  HashAlgorithm (CC.ADDRHASH (EraCrypto era)) =>
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

getScriptBabbageTxOut :: BabbageTxOut era -> StrictMaybe (Script era)
getScriptBabbageTxOut = \case
  TxOutCompact' {} -> SNothing
  TxOutCompactDH' {} -> SNothing
  TxOutCompactDatum {} -> SNothing
  TxOutCompactRefScript _ _ _ s -> SJust s
  TxOut_AddrHash28_AdaOnly {} -> SNothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> SNothing

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

getCompactValueBabbageTxOut :: EraTxOut era => BabbageTxOut era -> CompactForm (Value era)
getCompactValueBabbageTxOut =
  \case
    TxOutCompact' _ cv -> cv
    TxOutCompactDH' _ cv _ -> cv
    TxOutCompactDatum _ cv _ -> cv
    TxOutCompactRefScript _ cv _ _ -> cv
    TxOut_AddrHash28_AdaOnly _ _ cc -> injectCompact cc
    TxOut_AddrHash28_AdaOnly_DataHash32 _ _ cc _ -> injectCompact cc

txOutData :: Era era => BabbageTxOut era -> Maybe (Data era)
txOutData = strictMaybeToMaybe . getDataBabbageTxOut
{-# DEPRECATED txOutData "In favor of `dataTxOutL` or `getDataBabbageTxOut`" #-}

txOutDataHash :: Era era => BabbageTxOut era -> Maybe (DataHash (EraCrypto era))
txOutDataHash = strictMaybeToMaybe . getDataHashBabbageTxOut
{-# DEPRECATED txOutDataHash "In favor of `dataHashTxOutL` or `getDataHashBabbageTxOut`" #-}

txOutScript :: BabbageTxOut era -> Maybe (Script era)
txOutScript = strictMaybeToMaybe . getScriptBabbageTxOut
{-# DEPRECATED txOutScript "In favor of `dataTxOutL` or `getScriptBabbageTxOut`" #-}

decodeCIC :: (FromCBOR (Annotator b)) => T.Text -> Decoder s b
decodeCIC s = do
  version <- getDecoderVersion
  lbs <- decodeNestedCborBytes
  case decodeFullAnnotator version s fromCBOR (LBS.fromStrict lbs) of
    Left e -> fail $ T.unpack s <> ": " <> show e
    Right x -> pure x
