{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Babbage.TxBody
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH, TxOutCompactDatum),
    TxBody
      ( TxBody,
        inputs,
        collateral,
        referenceInputs,
        outputs,
        collateralReturn,
        totalCollateral,
        txcerts,
        txwdrls,
        txfee,
        txvldt,
        txUpdates,
        reqSignerHashes,
        mint,
        scriptIntegrityHash,
        adHash,
        txnetworkid
      ),
    Datum (..),
    datumDataHash,
    inputs',
    collateral',
    referenceInputs',
    outputs',
    collateralReturn',
    totalCollateral',
    certs',
    wdrls',
    txfee',
    vldt',
    update',
    reqSignerHashes',
    mint',
    scriptIntegrityHash',
    adHash',
    txnetworkid',
    getBabbageTxOutEitherAddr,
    BabbageBody,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeAnnotator,
    decodeBreakOr,
    decodeListLenOrIndef,
    encodeListLen,
  )
import Cardano.Crypto.Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data
  ( AuxiliaryDataHash (..),
    BinaryData,
    Data,
    DataHash,
    binaryDataToData,
    hashBinaryData,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( Addr28Extra,
    DataHash32,
    decodeAddress28,
    decodeDataHash32,
    encodeAddress28,
    encodeDataHash32,
    getAdaOnly,
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    isSNothing,
    maybeToStrictMaybe,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes
  ( EraIndependentScriptIntegrity,
    EraIndependentTxBody,
  )
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (Value (..), policies, policyID)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative,
    Val (..),
    decodeMint,
    decodeNonNegative,
    encodeMint,
    isZero,
  )
import Control.DeepSeq (NFData (rnf), rwhnf)
import Data.Coders
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing (FromSharedCBOR (..), Interns, fromNotSharedCBOR, interns)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), Typeable, (:~:) (Refl))
import Data.Word
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data TxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(DataHash (Crypto era))
  | TxOutCompactDatum
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
  | TxOutCompactRefScript'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(Datum era)
      !(Core.Script era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32

deriving stock instance
  ( Eq (Core.Value era),
    Eq (Core.Script era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxOut era)

-- | Already in NF
instance NFData (TxOut era) where
  rnf = rwhnf

viewCompactTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (CompactAddr (Crypto era), CompactForm (Core.Value era), StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, SJust dh)
  TxOutCompactDatum addr val datum -> (addr, val, SJust $ hashBinaryData datum)
  TxOutCompactRefScript' addr val datum _rs -> (addr, val, datumDataHash datum)
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal ->
    Alonzo.viewCompactTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32 ->
    Alonzo.viewCompactTxOut @era $
      Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32

viewTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, Datum era, StrictMaybe (Core.Script era))
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
viewTxOut (TxOutCompactRefScript' bs c d rs) = (addr, val, d, SJust rs)
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
  ( Era era,
    Show (Core.Value era),
    Show (Core.Script era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

data Datum era
  = NoDatum
  | DatumHash !(DataHash (Crypto era))
  | Datum !(BinaryData era)
  deriving (Eq, Ord, Show)

instance Era era => ToCBOR (Datum era) where
  toCBOR d = encode $ case d of
    NoDatum -> Sum NoDatum 0
    DatumHash dh -> Sum DatumHash 1 !> To dh
    Datum d' -> Sum Datum 2 !> To d'

instance Era era => FromCBOR (Datum era) where
  fromCBOR = decode (Summands "Datum" decodeDatum)
    where
      decodeDatum 0 = SumD NoDatum
      decodeDatum 1 = SumD DatumHash <! From
      decodeDatum 2 = SumD Datum <! From
      decodeDatum k = Invalid k

datumDataHash :: Era era => Datum era -> StrictMaybe (DataHash (Crypto era))
datumDataHash = \case
  NoDatum -> SNothing
  (DatumHash d) -> SJust d
  (Datum d) -> SJust $ hashBinaryData d

pattern TxOut ::
  forall era.
  ( Era era,
    Compactible (Core.Value era),
    Val (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  TxOut era
pattern TxOut addr vl datum refScript <-
  (viewTxOut -> (addr, vl, datum, refScript))
  where
    TxOut (Addr network paymentCred stakeRef) vl NoDatum SNothing
      | StakeRefBase stakeCred <- stakeRef,
        Just adaCompact <- getAdaOnly (Proxy @era) vl,
        Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
        TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
    TxOut (Addr network paymentCred stakeRef) vl (DatumHash dh) SNothing
      | StakeRefBase stakeCred <- stakeRef,
        Just adaCompact <- getAdaOnly (Proxy @era) vl,
        Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred,
        Just (Refl, dataHash32) <- encodeDataHash32 dh =
        TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
    TxOut addr vl d rs =
      let v = fromMaybe (error "Illegal value in txout") $ toCompact vl
          a = compactAddr addr
       in case rs of
            SNothing -> case d of
              NoDatum -> TxOutCompact' a v
              DatumHash dh -> TxOutCompactDH' a v dh
              Datum binaryData -> TxOutCompactDatum a v binaryData
            SJust rs' -> TxOutCompactRefScript' a v d rs'

{-# COMPLETE TxOut #-}

-- TODO deprecate
pattern TxOutCompact ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  TxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, SNothing))
  where
    TxOutCompact cAddr cVal = TxOut (decompactAddr cAddr) (fromCompact cVal) NoDatum SNothing

-- TODO deprecate
pattern TxOutCompactDH ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  DataHash (Crypto era) ->
  TxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, SJust dh))
  where
    TxOutCompactDH cAddr cVal dh = TxOut (decompactAddr cAddr) (fromCompact cVal) (DatumHash dh) SNothing

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _spendInputs :: !(Set (TxIn (Crypto era))),
    _collateralInputs :: !(Set (TxIn (Crypto era))),
    _referenceInputs :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (TxOut era)),
    _collateralReturn :: !(StrictMaybe (TxOut era)),
    _totalCollateral :: !Coin,
    _certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: Set (KeyHash 'Witness (Crypto era)),
    _mint :: !(Value (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.Value, not a Core.Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (Crypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  ( Eq (Core.Value era),
    Eq (Core.Script era),
    CC.Crypto (Crypto era),
    Compactible (Core.Value era),
    Eq (PParamsDelta era)
  ) =>
  Eq (TxBodyRaw era)

instance
  (Typeable era, NoThunks (Core.Value era), NoThunks (PParamsDelta era)) =>
  NoThunks (TxBodyRaw era)

deriving instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.Script era),
    Show (PParamsDelta era)
  ) =>
  Show (TxBodyRaw era)

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  ( CC.Crypto (Crypto era)
  ) =>
  Eq (TxBody era)

deriving instance
  ( Typeable era,
    NoThunks (Core.Value era),
    NoThunks (PParamsDelta era)
  ) =>
  NoThunks (TxBody era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (PParamsDelta era)
  ) =>
  Show (TxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    ( Era era,
      Typeable (Core.Script era),
      Typeable (Core.AuxiliaryData era),
      Compactible (Core.Value era),
      Show (Core.Value era),
      DecodeNonNegative (Core.Value era),
      FromCBOR (Annotator (Core.Script era)),
      Core.SerialisableData (PParamsDelta era)
    ) =>
    FromCBOR (Annotator (TxBody era))

instance
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    DecodeNonNegative (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (PParamsDelta era),
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

-- The Set of constraints necessary to use the TxBody pattern
type BabbageBody era =
  ( Era era,
    Compactible (Core.Value era),
    ToCBOR (Core.Script era),
    Core.SerialisableData (PParamsDelta era)
  )

pattern TxBody ::
  BabbageBody era =>
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictMaybe (TxOut era) ->
  Coin ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness (Crypto era)) ->
  Value (Crypto era) ->
  StrictMaybe (ScriptIntegrityHash (Crypto era)) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  StrictMaybe Network ->
  TxBody era
pattern TxBody
  { inputs,
    collateral,
    referenceInputs,
    outputs,
    collateralReturn,
    totalCollateral,
    txcerts,
    txwdrls,
    txfee,
    txvldt,
    txUpdates,
    reqSignerHashes,
    mint,
    scriptIntegrityHash,
    adHash,
    txnetworkid
  } <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { _spendInputs = inputs,
            _collateralInputs = collateral,
            _referenceInputs = referenceInputs,
            _outputs = outputs,
            _collateralReturn = collateralReturn,
            _totalCollateral = totalCollateral,
            _certs = txcerts,
            _wdrls = txwdrls,
            _txfee = txfee,
            _vldt = txvldt,
            _update = txUpdates,
            _reqSignerHashes = reqSignerHashes,
            _mint = mint,
            _scriptIntegrityHash = scriptIntegrityHash,
            _adHash = adHash,
            _txnetworkid = txnetworkid
          }
        _
      )
  where
    TxBody
      inputsX
      collateralX
      referenceInputsX
      outputsX
      collateralReturnX
      totalCollateralX
      certsX
      wdrlsX
      txfeeX
      vldtX
      updateX
      reqSignerHashesX
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX =
        TxBodyConstr $
          memoBytes
            ( encodeTxBodyRaw $
                TxBodyRaw
                  inputsX
                  collateralX
                  referenceInputsX
                  outputsX
                  collateralReturnX
                  totalCollateralX
                  certsX
                  wdrlsX
                  txfeeX
                  vldtX
                  updateX
                  reqSignerHashesX
                  mintX
                  scriptIntegrityHashX
                  adHashX
                  txnetworkidX
            )

{-# COMPLETE TxBody #-}

instance (c ~ Crypto era) => HashAnnotated (TxBody era) EraIndependentTxBody c

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: TxBody era -> Set (TxIn (Crypto era))
collateral' :: TxBody era -> Set (TxIn (Crypto era))
referenceInputs' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (TxOut era)
collateralReturn' :: TxBody era -> StrictMaybe (TxOut era)
totalCollateral' :: TxBody era -> Coin
certs' :: TxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: TxBody era -> Coin
wdrls' :: TxBody era -> Wdrl (Crypto era)
vldt' :: TxBody era -> ValidityInterval
update' :: TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: TxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: TxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: TxBody era -> Value (Crypto era)
scriptIntegrityHash' :: TxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
inputs' (TxBodyConstr (Memo raw _)) = _spendInputs raw

txnetworkid' :: TxBody era -> StrictMaybe Network

collateral' (TxBodyConstr (Memo raw _)) = _collateralInputs raw

referenceInputs' (TxBodyConstr (Memo raw _)) = _referenceInputs raw

outputs' (TxBodyConstr (Memo raw _)) = _outputs raw

collateralReturn' (TxBodyConstr (Memo raw _)) = _collateralReturn raw

totalCollateral' (TxBodyConstr (Memo raw _)) = _totalCollateral raw

certs' (TxBodyConstr (Memo raw _)) = _certs raw

wdrls' (TxBodyConstr (Memo raw _)) = _wdrls raw

txfee' (TxBodyConstr (Memo raw _)) = _txfee raw

vldt' (TxBodyConstr (Memo raw _)) = _vldt raw

update' (TxBodyConstr (Memo raw _)) = _update raw

reqSignerHashes' (TxBodyConstr (Memo raw _)) = _reqSignerHashes raw

adHash' (TxBodyConstr (Memo raw _)) = _adHash raw

mint' (TxBodyConstr (Memo raw _)) = _mint raw

scriptIntegrityHash' (TxBodyConstr (Memo raw _)) = _scriptIntegrityHash raw

txnetworkid' (TxBodyConstr (Memo raw _)) = _txnetworkid raw

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  ( Era era,
    Compactible (Core.Value era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr cv) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR cv
  toCBOR (TxOutCompactDatum addr cv d) =
    encodeListLen 4
      <> toCBOR (0 :: Word8)
      <> toCBOR addr
      <> toCBOR cv
      <> toCBOR d
  toCBOR (TxOutCompactRefScript' addr cv d rs) =
    encodeListLen 5
      <> toCBOR (2 :: Word8)
      <> toCBOR addr
      <> toCBOR cv
      <> toCBOR d
      <> toCBOR rs
  toCBOR (TxOutCompactDH addr cv dh) =
    encodeListLen 3
      <> toCBOR addr
      <> toCBOR cv
      <> toCBOR dh

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    Compactible (Core.Value era)
  ) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = fromNotSharedCBOR

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    Compactible (Core.Value era)
  ) =>
  FromSharedCBOR (TxOut era)
  where
  type Share (TxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns = do
    lenOrIndef <- decodeListLenOrIndef
    let internTxOut = \case
          TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
            TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada
          TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
            TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32
          txOut -> txOut
    internTxOut <$> case lenOrIndef of
      Nothing -> do
        a <- fromCBOR
        cv <- decodeNonNegative
        decodeBreakOr >>= \case
          True -> pure $ TxOutCompact a cv
          False -> do
            dh <- fromCBOR
            decodeBreakOr >>= \case
              True -> pure $ TxOutCompactDH a cv dh
              False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
      Just 2 ->
        TxOutCompact
          <$> fromCBOR
          <*> decodeNonNegative
      Just 3 ->
        TxOutCompactDH
          <$> fromCBOR
          <*> decodeNonNegative
          <*> fromCBOR
      Just 4 -> do
        1 <- fromCBOR @Word8
        TxOutCompactDatum <$> fromCBOR <*> decodeNonNegative <*> fromCBOR
      Just 5 -> do
        1 <- fromCBOR @Word8
        TxOutCompactRefScript' <$> fromCBOR <*> decodeNonNegative <*> fromCBOR <*> decodeCIC "Script"
      Just n -> cborError $ DecoderErrorCustom "txout" $ "wrong number of terms in txout: " <> T.pack (show n)

decodeCIC :: (FromCBOR (Annotator b)) => T.Text -> Decoder s b
decodeCIC s = do
  lbs <- fromCBOR
  case decodeAnnotator s fromCBOR lbs of
    Left _ -> fail "foo"
    Right x -> pure x

encodeTxBodyRaw ::
  ( Era era,
    ToCBOR (PParamsDelta era),
    ToCBOR (Core.Script era)
  ) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw
  TxBodyRaw
    { _spendInputs,
      _collateralInputs,
      _referenceInputs,
      _outputs,
      _collateralReturn,
      _totalCollateral,
      _certs,
      _wdrls,
      _txfee,
      _vldt = ValidityInterval bot top,
      _update,
      _reqSignerHashes,
      _mint,
      _scriptIntegrityHash,
      _adHash,
      _txnetworkid
    } =
    Keyed
      ( \i ifee ri o cr tc f t c w u b rsh mi sh ah ni ->
          TxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) u rsh mi sh ah ni
      )
      !> Key 0 (E encodeFoldable _spendInputs)
      !> Key 13 (E encodeFoldable _collateralInputs)
      !> Key 18 (E encodeFoldable _referenceInputs)
      !> Key 1 (E encodeFoldable _outputs)
      !> Key 16 (To _collateralReturn)
      !> Key 17 (To _totalCollateral)
      !> Key 2 (To _txfee)
      !> encodeKeyedStrictMaybe 3 top
      !> Omit null (Key 4 (E encodeFoldable _certs))
      !> Omit (null . unWdrl) (Key 5 (To _wdrls))
      !> encodeKeyedStrictMaybe 6 _update
      !> encodeKeyedStrictMaybe 8 bot
      !> Key 14 (E encodeFoldable _reqSignerHashes)
      !> Omit isZero (Key 9 (E encodeMint _mint))
      !> encodeKeyedStrictMaybe 11 _scriptIntegrityHash
      !> encodeKeyedStrictMaybe 7 _adHash
      !> encodeKeyedStrictMaybe 15 _txnetworkid
    where
      encodeKeyedStrictMaybe key x =
        Omit isSNothing (Key key (E (toCBOR . fromSJust) x))

      fromSJust :: StrictMaybe a -> a
      fromSJust (SJust x) = x
      fromSJust SNothing = error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing"

instance
  forall era.
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    DecodeNonNegative (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (PParamsDelta era),
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        initial
        bodyFields
        requiredFields
    where
      initial :: TxBodyRaw era
      initial =
        TxBodyRaw
          mempty
          mempty
          mempty
          StrictSeq.empty
          SNothing
          mempty
          StrictSeq.empty
          (Wdrl mempty)
          mempty
          (ValidityInterval SNothing SNothing)
          SNothing
          mempty
          mempty
          SNothing
          SNothing
          SNothing
      bodyFields :: (Word -> Field (TxBodyRaw era))
      bodyFields 0 =
        field
          (\x tx -> tx {_spendInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        field
          (\x tx -> tx {_collateralInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 18 =
        field
          (\x tx -> tx {_referenceInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        field
          (\x tx -> tx {_outputs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 16 =
        field
          (\x tx -> tx {_collateralReturn = x})
          (D fromCBOR)
      bodyFields 17 =
        field
          (\x tx -> tx {_totalCollateral = x})
          (D fromCBOR)
      bodyFields 2 = field (\x tx -> tx {_txfee = x}) From
      bodyFields 3 =
        field
          (\x tx -> tx {_vldt = (_vldt tx) {invalidHereafter = x}})
          (D (SJust <$> fromCBOR))
      bodyFields 4 =
        field
          (\x tx -> tx {_certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 = field (\x tx -> tx {_wdrls = x}) From
      bodyFields 6 = field (\x tx -> tx {_update = x}) (D (SJust <$> fromCBOR))
      bodyFields 7 = field (\x tx -> tx {_adHash = x}) (D (SJust <$> fromCBOR))
      bodyFields 8 =
        field
          (\x tx -> tx {_vldt = (_vldt tx) {invalidBefore = x}})
          (D (SJust <$> fromCBOR))
      bodyFields 9 = field (\x tx -> tx {_mint = x}) (D decodeMint)
      bodyFields 11 = field (\x tx -> tx {_scriptIntegrityHash = x}) (D (SJust <$> fromCBOR))
      bodyFields 14 = field (\x tx -> tx {_reqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = field (\x tx -> tx {_txnetworkid = x}) (D (SJust <$> fromCBOR))
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

-- ====================================================
-- HasField instances to be consistent with earlier Eras

instance (Crypto era ~ c) => HasField "inputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _spendInputs m

instance HasField "outputs" (TxBody era) (StrictSeq (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = _outputs m

instance Crypto era ~ crypto => HasField "certs" (TxBody era) (StrictSeq (DCert crypto)) where
  getField (TxBodyConstr (Memo m _)) = _certs m

instance Crypto era ~ crypto => HasField "wdrls" (TxBody era) (Wdrl crypto) where
  getField (TxBodyConstr (Memo m _)) = _wdrls m

instance HasField "txfee" (TxBody era) Coin where
  getField (TxBodyConstr (Memo m _)) = _txfee m

instance HasField "update" (TxBody era) (StrictMaybe (Update era)) where
  getField (TxBodyConstr (Memo m _)) = _update m

instance
  (Crypto era ~ c) =>
  HasField "reqSignerHashes" (TxBody era) (Set (KeyHash 'Witness c))
  where
  getField (TxBodyConstr (Memo m _)) = _reqSignerHashes m

instance (Crypto era ~ c) => HasField "mint" (TxBody era) (Mary.Value c) where
  getField (TxBodyConstr (Memo m _)) = _mint m

instance (Crypto era ~ c) => HasField "collateral" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _collateralInputs m

instance (Crypto era ~ c) => HasField "referenceInputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _referenceInputs m

instance HasField "collateralReturn" (TxBody era) (StrictMaybe (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = _collateralReturn m

instance HasField "totalCollateral" (TxBody era) Coin where
  getField (TxBodyConstr (Memo m _)) = _totalCollateral m

instance (Crypto era ~ c) => HasField "minted" (TxBody era) (Set (ScriptHash c)) where
  getField (TxBodyConstr (Memo m _)) = Set.map policyID (policies (_mint m))

instance HasField "vldt" (TxBody era) ValidityInterval where
  getField (TxBodyConstr (Memo m _)) = _vldt m

instance
  c ~ Crypto era =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _adHash m

instance
  c ~ Crypto era =>
  HasField "scriptIntegrityHash" (TxBody era) (StrictMaybe (ScriptIntegrityHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _scriptIntegrityHash m

instance HasField "txnetworkid" (TxBody era) (StrictMaybe Network) where
  getField (TxBodyConstr (Memo m _)) = _txnetworkid m

instance (Era era, Core.Value era ~ val, Compactible val) => HasField "value" (TxOut era) val where
  getField (TxOutCompact _ v) = fromCompact v
  getField (TxOutCompactDH _ v _) = fromCompact v

instance (Era era, c ~ Crypto era) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField = maybeToStrictMaybe . txOutDataHash

instance (Era era) => HasField "datum" (TxOut era) (StrictMaybe (Data era)) where
  getField = maybeToStrictMaybe . txOutData

getBabbageTxOutEitherAddr ::
  HashAlgorithm (CC.ADDRHASH (Crypto era)) =>
  TxOut era ->
  Either (Addr (Crypto era)) (CompactAddr (Crypto era))
getBabbageTxOutEitherAddr = \case
  TxOutCompact' cAddr _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ -> Right cAddr
  TxOutCompactDatum cAddr _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address of non-standard size"
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
  _ -> error "Impossible: Compacted an address or a hash of non-standard size"

txOutData :: TxOut era -> Maybe (Data era)
txOutData = \case
  TxOutCompactDatum _ _ binaryData -> Just $! binaryDataToData binaryData
  _ -> Nothing

txOutDataHash :: Era era => TxOut era -> Maybe (DataHash (Crypto era))
txOutDataHash = \case
  TxOutCompactDH' _ _ dh -> Just dh
  TxOutCompactDatum _ _ binaryData -> Just $! hashBinaryData binaryData
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32 -> decodeDataHash32 dataHash32
  _ -> Nothing
