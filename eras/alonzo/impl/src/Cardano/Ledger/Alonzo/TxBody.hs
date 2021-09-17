{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Alonzo.TxBody
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH),
    TxBody
      ( TxBody,
        inputs,
        collateral,
        outputs,
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
    inputs',
    collateral',
    outputs',
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
    AlonzoBody,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
    ppTxBody,
    ppTxOut,

    -- * deprecated
    WitnessPPDataHash,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeBreakOr,
    decodeListLenOrIndef,
    encodeListLen,
  )
import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..), DataHash)
import Cardano.Ledger.BaseTypes
  ( Network,
    StrictMaybe (..),
    isSNothing,
  )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes
  ( EraIndependentScriptIntegrity,
    EraIndependentTxBody,
  )
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (Value (..), policies, policyID, ppValue)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppAddr,
    ppCoin,
    ppDCert,
    ppKeyHash,
    ppNetwork,
    ppRecord,
    ppSafeHash,
    ppSet,
    ppSexp,
    ppStrictMaybe,
    ppStrictSeq,
    ppTxIn,
    ppUpdate,
    ppWdrl,
  )
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr, compactAddr, decompactAddr)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import Cardano.Ledger.Shelley.TxBody (TxIn (..), Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..), ppValidityInterval)
import Cardano.Ledger.Val
  ( DecodeNonNegative,
    decodeMint,
    decodeNonNegative,
    encodeMint,
    isZero,
  )
import Data.Coders
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data TxOut era
  = TxOutCompact
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
  | TxOutCompactDH
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      {-# UNPACK #-} !(DataHash (Crypto era))

deriving stock instance
  ( Eq (Core.Value era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxOut era)

viewCompactTxOut ::
  forall era.
  (Era era) =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, StrictMaybe (DataHash (Crypto era)))
viewCompactTxOut (TxOutCompact bs c) = (addr, val, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewCompactTxOut (TxOutCompactDH bs c dh) = (addr, val, SJust dh)
  where
    addr = decompactAddr bs
    val = fromCompact c

instance
  ( Era era,
    Show (Core.Value era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewCompactTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

pattern TxOut ::
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  StrictMaybe (DataHash (Crypto era)) ->
  TxOut era
pattern TxOut addr vl dh <-
  (viewCompactTxOut -> (addr, vl, dh))
  where
    TxOut addr vl mdh =
      let v = fromMaybe (error $ "Illegal value in txout: " <> show vl) $ toCompact vl
          a = compactAddr addr
       in case mdh of
            SNothing -> TxOutCompact a v
            SJust dh -> TxOutCompactDH a v dh

{-# COMPLETE TxOut #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

{-# DEPRECATED WitnessPPDataHash "Use ScriptIntegrityHash instead" #-}

type WitnessPPDataHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { _inputs :: !(Set (TxIn (Crypto era))),
    _collateral :: !(Set (TxIn (Crypto era))),
    _outputs :: !(StrictSeq (TxOut era)),
    _certs :: !(StrictSeq (DCert (Crypto era))),
    _wdrls :: !(Wdrl (Crypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: Set (KeyHash 'Witness (Crypto era)),
    _mint :: !(Value (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.Value, not a Core.Value.
    -- Operations on the TxBody in the AlonzoEra depend upon this.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (Crypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  ( Eq (Core.Value era),
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
    Show (PParamsDelta era)
  ) =>
  Show (TxBodyRaw era)

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  ( Eq (Core.Value era),
    Compactible (Core.Value era),
    CC.Crypto (Crypto era),
    Eq (PParamsDelta era)
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

-- The Set of constraints necessary to use the TxBody pattern
type AlonzoBody era =
  ( Era era,
    Compactible (Core.Value era),
    ToCBOR (Core.Script era),
    Core.SerialisableData (PParamsDelta era)
  )

pattern TxBody ::
  AlonzoBody era =>
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
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
    outputs,
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
          { _inputs = inputs,
            _collateral = collateral,
            _outputs = outputs,
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
      outputsX
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
                  outputsX
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
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: TxBody era -> Set (TxIn (Crypto era))
collateral' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (TxOut era)
certs' :: TxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: TxBody era -> Coin
wdrls' :: TxBody era -> Wdrl (Crypto era)
vldt' :: TxBody era -> ValidityInterval
update' :: TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: TxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: TxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: TxBody era -> Value (Crypto era)
scriptIntegrityHash' :: TxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
inputs' (TxBodyConstr (Memo raw _)) = _inputs raw

txnetworkid' :: TxBody era -> StrictMaybe Network

collateral' (TxBodyConstr (Memo raw _)) = _collateral raw

outputs' (TxBodyConstr (Memo raw _)) = _outputs raw

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
    Compactible (Core.Value era)
  ) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOutCompact addr cv) =
    encodeListLen 2
      <> toCBOR addr
      <> toCBOR cv
  toCBOR (TxOutCompactDH addr cv dh) =
    encodeListLen 3
      <> toCBOR addr
      <> toCBOR cv
      <> toCBOR dh

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
      Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"

encodeTxBodyRaw ::
  ( Era era,
    ToCBOR (PParamsDelta era)
  ) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw
  TxBodyRaw
    { _inputs,
      _collateral,
      _outputs,
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
      ( \i ifee o f t c w u b rsh mi sh ah ni ->
          TxBodyRaw i ifee o c w f (ValidityInterval b t) u rsh mi sh ah ni
      )
      !> Key 0 (E encodeFoldable _inputs)
      !> Key 13 (E encodeFoldable _collateral)
      !> Key 1 (E encodeFoldable _outputs)
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
          StrictSeq.empty
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
          (\x tx -> tx {_inputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        field
          (\x tx -> tx {_collateral = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        field
          (\x tx -> tx {_outputs = x})
          (D (decodeStrictSeq fromCBOR))
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

-- ====================================================
-- HasField instances to be consistent with earlier Eras

instance (Crypto era ~ c) => HasField "inputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = _inputs m

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
  getField (TxBodyConstr (Memo m _)) = _collateral m

instance (Crypto era ~ c) => HasField "minted" (TxBody era) (Set (ScriptHash c)) where
  getField (TxBodyConstr (Memo m _)) = Set.map policyID (policies (_mint m))

instance HasField "vldt" (TxBody era) ValidityInterval where
  getField (TxBodyConstr (Memo m _)) = _vldt m

instance
  c ~ Crypto era =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _adHash m

-- | TODO deprecated
instance
  c ~ Crypto era =>
  HasField "wppHash" (TxBody era) (StrictMaybe (ScriptIntegrityHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _scriptIntegrityHash m

instance
  c ~ Crypto era =>
  HasField "scriptIntegrityHash" (TxBody era) (StrictMaybe (ScriptIntegrityHash c))
  where
  getField (TxBodyConstr (Memo m _)) = _scriptIntegrityHash m

instance HasField "txnetworkid" (TxBody era) (StrictMaybe Network) where
  getField (TxBodyConstr (Memo m _)) = _txnetworkid m

instance (Crypto era ~ c) => HasField "compactAddress" (TxOut era) (CompactAddr c) where
  getField (TxOutCompact a _) = a
  getField (TxOutCompactDH a _ _) = a

instance (CC.Crypto c, Crypto era ~ c) => HasField "address" (TxOut era) (Addr c) where
  getField (TxOutCompact a _) = decompactAddr a
  getField (TxOutCompactDH a _ _) = decompactAddr a

instance (Core.Value era ~ val, Compactible val) => HasField "value" (TxOut era) val where
  getField (TxOutCompact _ v) = fromCompact v
  getField (TxOutCompactDH _ v _) = fromCompact v

instance c ~ Crypto era => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField (TxOutCompact _ _) = SNothing
  getField (TxOutCompactDH _ _ d) = SJust d

-- ===================================================

ppTxOut ::
  ( Era era,
    Show (Core.Value era),
    PrettyA (Core.Value era)
  ) =>
  TxOut era ->
  PDoc
ppTxOut (TxOut addr val dhash) =
  ppSexp "TxOut" [ppAddr addr, prettyA val, ppStrictMaybe ppSafeHash dhash]

ppTxBody ::
  ( Era era,
    Show (Core.Value era),
    PrettyA (Core.Value era),
    PrettyA (PParamsDelta era)
  ) =>
  TxBody era ->
  PDoc
ppTxBody (TxBodyConstr (Memo (TxBodyRaw i ifee o c w fee vi u rsh mnt sdh axh ni) _)) =
  ppRecord
    "TxBody(Alonzo)"
    [ ("inputs", ppSet ppTxIn i),
      ("collateral", ppSet ppTxIn ifee),
      ("outputs", ppStrictSeq ppTxOut o),
      ("certificates", ppStrictSeq ppDCert c),
      ("withdrawals", ppWdrl w),
      ("txfee", ppCoin fee),
      ("vldt", ppValidityInterval vi),
      ("update", ppStrictMaybe ppUpdate u),
      ("reqSignerHashes", ppSet ppKeyHash rsh),
      ("mint", ppValue mnt),
      ("scriptIntegrityHash", ppStrictMaybe ppSafeHash sdh),
      ("adHash", ppStrictMaybe ppAuxDataHash axh),
      ("txnetworkid", ppStrictMaybe ppNetwork ni)
    ]

ppAuxDataHash :: AuxiliaryDataHash crypto -> PDoc
ppAuxDataHash (AuxiliaryDataHash axh) = ppSafeHash axh

instance
  ( Era era,
    PrettyA (Core.Value era),
    PrettyA (PParamsDelta era),
    Show (Core.Value era)
  ) =>
  PrettyA (TxBody era)
  where
  prettyA = ppTxBody

instance
  ( Era era,
    Show (Core.Value era),
    PrettyA (Core.Value era)
  ) =>
  PrettyA (TxOut era)
  where
  prettyA x = ppTxOut x
