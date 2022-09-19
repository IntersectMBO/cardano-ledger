{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxBody
  ( AlonzoTxOut (..),
    AlonzoEraTxOut (..),
    -- Constructors are not exported for safety:
    Addr28Extra,
    DataHash32,
    AlonzoTxBody
      ( AlonzoTxBody,
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
    AlonzoEraTxBody (..),
    ShelleyEraTxBody (..),
    ShelleyMAEraTxBody (..),
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
    getAdaOnly,
    decodeDataHash32,
    encodeDataHash32,
    encodeAddress28,
    decodeAddress28,
    viewCompactTxOut,
    viewTxOut,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
    getAlonzoTxOutEitherAddr,
    utxoEntrySize,
    txBodyRawEq,

    -- * Deprecated
    TxOut,
    TxBody,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxBody (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..))
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.PParams.Class (AlonzoEraPParams)
import Cardano.Ledger.Alonzo.Scripts ()
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset (..), policies, policyID)
import Cardano.Ledger.MemoBytes (Mem, MemoBytes (..), MemoHashIndex, contentsEq, memoBytes)
import Cardano.Ledger.SafeHash
  ( HashAnnotated (..),
    SafeToHash,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (DecodeNonNegative, Val (..), decodeMint, encodeMint)
import Control.DeepSeq (NFData (..))
import Data.Coders hiding (to)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)
import Prelude hiding (lookup)

-- ======================================

data TxBodyRaw era = TxBodyRaw
  { _inputs :: !(Set (TxIn (EraCrypto era))),
    _collateral :: !(Set (TxIn (EraCrypto era))),
    _outputs :: !(StrictSeq (Core.TxOut era)),
    _certs :: !(StrictSeq (DCert (EraCrypto era))),
    _wdrls :: !(Wdrl (EraCrypto era)),
    _txfee :: !Coin,
    _vldt :: !ValidityInterval,
    _update :: !(StrictMaybe (Update era)),
    _reqSignerHashes :: Set (KeyHash 'Witness (EraCrypto era)),
    _mint :: !(MultiAsset (EraCrypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Cardano.Ledger.Core.Value.
    -- Operations on the TxBody in the AlonzoEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    _scriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era))),
    _adHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    _txnetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  ( Era era,
    Eq (Core.TxOut era),
    Eq (PParamsUpdate era),
    Eq (Value era),
    Compactible (Value era),
    Eq (PParamsHKD StrictMaybe era)
  ) =>
  Eq (TxBodyRaw era)

instance (Era era, NoThunks (Core.TxOut era), NoThunks (PParamsUpdate era)) => NoThunks (TxBodyRaw era)

instance (Era era, NFData (Core.TxOut era), NFData (PParamsUpdate era)) => NFData (TxBodyRaw era)

deriving instance
  (Era era, Show (Core.TxOut era), Show (PParamsUpdate era), Show (Value era), Val (Value era)) =>
  Show (TxBodyRaw era)

newtype AlonzoTxBody era = TxBodyConstr (MemoBytes TxBodyRaw era)
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

type TxBody era = AlonzoTxBody era

{-# DEPRECATED TxBody "Use `AlonzoTxBody` instead" #-}

lensTxBodyRaw ::
  (Era era, ToCBOR (Core.TxOut era), ToCBOR (PParamsUpdate era)) =>
  (TxBodyRaw era -> a) ->
  (TxBodyRaw era -> t -> TxBodyRaw era) ->
  Lens (AlonzoTxBody era) (AlonzoTxBody era) a t
lensTxBodyRaw getter setter =
  lens
    (\(TxBodyConstr (Memo txBodyRaw _)) -> getter txBodyRaw)
    (\(TxBodyConstr (Memo txBodyRaw _)) val -> mkAlonzoTxBody $ setter txBodyRaw val)
{-# INLINEABLE lensTxBodyRaw #-}

instance
  ( AlonzoEraPParams (AlonzoEra c),
    EraPParams (AlonzoEra c),
    Val (MaryValue c),
    DecodeNonNegative (MaryValue c)
  ) =>
  EraTxBody (AlonzoEra c)
  where
  {-# SPECIALIZE instance AlonzoEraPParams (AlonzoEra CC.StandardCrypto) => EraTxBody (AlonzoEra CC.StandardCrypto) #-}

  type TxBody (AlonzoEra c) = AlonzoTxBody (AlonzoEra c)

  mkBasicTxBody = mkAlonzoTxBody initial

  inputsTxBodyL =
    lensTxBodyRaw _inputs (\txBodyRaw inputs_ -> txBodyRaw {_inputs = inputs_})
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensTxBodyRaw _outputs (\txBodyRaw outputs_ -> txBodyRaw {_outputs = outputs_})
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensTxBodyRaw _txfee (\txBodyRaw fee_ -> txBodyRaw {_txfee = fee_})
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensTxBodyRaw _adHash (\txBodyRaw auxDataHash -> txBodyRaw {_adHash = auxDataHash})
  {-# INLINEABLE auxDataHashTxBodyL #-}

  allInputsTxBodyF =
    to $ \txBody -> (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL)
  {-# INLINEABLE allInputsTxBodyF #-}

instance (CC.Crypto c, AlonzoEraPParams (AlonzoEra c)) => ShelleyEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraPParams (AlonzoEra CC.StandardCrypto) => ShelleyEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  wdrlsTxBodyL =
    lensTxBodyRaw _wdrls (\txBodyRaw wdrls_ -> txBodyRaw {_wdrls = wdrls_})
  {-# INLINEABLE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL

  updateTxBodyL =
    lensTxBodyRaw _update (\txBodyRaw update_ -> txBodyRaw {_update = update_})
  {-# INLINEABLE updateTxBodyL #-}

  certsTxBodyL =
    lensTxBodyRaw _certs (\txBodyRaw certs_ -> txBodyRaw {_certs = certs_})
  {-# INLINEABLE certsTxBodyL #-}

instance (CC.Crypto c, AlonzoEraPParams (AlonzoEra c)) => ShelleyMAEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraPParams (AlonzoEra CC.StandardCrypto) => ShelleyMAEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  vldtTxBodyL =
    lensTxBodyRaw _vldt (\txBodyRaw vldt_ -> txBodyRaw {_vldt = vldt_})
  {-# INLINEABLE vldtTxBodyL #-}

  mintTxBodyL =
    lensTxBodyRaw _mint (\txBodyRaw mint_ -> txBodyRaw {_mint = mint_})
  {-# INLINEABLE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue 0)
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF =
    to (\(TxBodyConstr (Memo txBodyRaw _)) -> Set.map policyID (policies (_mint txBodyRaw)))
  {-# INLINEABLE mintedTxBodyF #-}

instance (CC.Crypto c, AlonzoEraPParams (AlonzoEra c)) => AlonzoEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraPParams (AlonzoEra CC.StandardCrypto) => AlonzoEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  collateralInputsTxBodyL =
    lensTxBodyRaw _collateral (\txBodyRaw collateral_ -> txBodyRaw {_collateral = collateral_})
  {-# INLINEABLE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensTxBodyRaw
      _reqSignerHashes
      (\txBodyRaw reqSignerHashes_ -> txBodyRaw {_reqSignerHashes = reqSignerHashes_})
  {-# INLINEABLE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensTxBodyRaw
      _scriptIntegrityHash
      (\txBodyRaw scriptIntegrityHash_ -> txBodyRaw {_scriptIntegrityHash = scriptIntegrityHash_})
  {-# INLINEABLE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL =
    lensTxBodyRaw _txnetworkid (\txBodyRaw networkId -> txBodyRaw {_txnetworkid = networkId})
  {-# INLINEABLE networkIdTxBodyL #-}

deriving newtype instance CC.Crypto (EraCrypto era) => Eq (AlonzoTxBody era)

deriving instance
  (Era era, NoThunks (Core.TxOut era), NoThunks (PParamsUpdate era)) =>
  NoThunks (AlonzoTxBody era)

deriving instance
  (Era era, NFData (Core.TxOut era), NFData (PParamsUpdate era)) =>
  NFData (AlonzoTxBody era)

deriving instance
  (Era era, Show (Core.TxOut era), Show (PParamsUpdate era), Show (Value era), Val (Value era)) =>
  Show (AlonzoTxBody era)

deriving via
  (Mem TxBodyRaw era)
  instance
    ( Era era,
      FromCBOR (Core.TxOut era),
      FromCBOR (PParamsUpdate era),
      Show (Value era)
    ) =>
    FromCBOR (Annotator (AlonzoTxBody era))

pattern AlonzoTxBody ::
  (EraTxOut era, ToCBOR (PParamsUpdate era)) =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (Core.TxOut era) ->
  StrictSeq (DCert (EraCrypto era)) ->
  Wdrl (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness (EraCrypto era)) ->
  MultiAsset (EraCrypto era) ->
  StrictMaybe (ScriptIntegrityHash (EraCrypto era)) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  StrictMaybe Network ->
  AlonzoTxBody era
pattern AlonzoTxBody
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
    AlonzoTxBody
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
        mkAlonzoTxBody $
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

{-# COMPLETE AlonzoTxBody #-}

mkAlonzoTxBody ::
  (Era era, ToCBOR (Core.TxOut era), ToCBOR (PParamsUpdate era)) =>
  TxBodyRaw era ->
  AlonzoTxBody era
mkAlonzoTxBody = TxBodyConstr . memoBytes . encodeTxBodyRaw

type instance MemoHashIndex TxBodyRaw = EraIndependentTxBody

instance (c ~ EraCrypto era) => HashAnnotated (AlonzoTxBody era) EraIndependentTxBody c where
  hashAnnotated (TxBodyConstr mb) = mbHash mb

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: Era era => AlonzoTxBody era -> Set (TxIn (EraCrypto era))
collateral' :: Era era => AlonzoTxBody era -> Set (TxIn (EraCrypto era))
outputs' :: Era era => AlonzoTxBody era -> StrictSeq (Core.TxOut era)
certs' :: Era era => AlonzoTxBody era -> StrictSeq (DCert (EraCrypto era))
txfee' :: Era era => AlonzoTxBody era -> Coin
wdrls' :: Era era => AlonzoTxBody era -> Wdrl (EraCrypto era)
vldt' :: Era era => AlonzoTxBody era -> ValidityInterval
update' :: Era era => AlonzoTxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: Era era => AlonzoTxBody era -> Set (KeyHash 'Witness (EraCrypto era))
adHash' :: Era era => AlonzoTxBody era -> StrictMaybe (AuxiliaryDataHash (EraCrypto era))
mint' :: Era era => AlonzoTxBody era -> MultiAsset (EraCrypto era)
scriptIntegrityHash' :: Era era => AlonzoTxBody era -> StrictMaybe (ScriptIntegrityHash (EraCrypto era))
txnetworkid' :: Era era => AlonzoTxBody era -> StrictMaybe Network
inputs' (TxBodyConstr (Memo raw _)) = _inputs raw

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

encodeTxBodyRaw ::
  ( Era era,
    ToCBOR (Core.TxOut era),
    ToCBOR (PParamsUpdate era)
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
      !> Omit null (Key 13 (E encodeFoldable _collateral))
      !> Key 1 (E encodeFoldable _outputs)
      !> Key 2 (To _txfee)
      !> encodeKeyedStrictMaybe 3 top
      !> Omit null (Key 4 (E encodeFoldable _certs))
      !> Omit (null . unWdrl) (Key 5 (To _wdrls))
      !> encodeKeyedStrictMaybe 6 _update
      !> encodeKeyedStrictMaybe 8 bot
      !> Omit null (Key 14 (E encodeFoldable _reqSignerHashes))
      !> Omit (== mempty) (Key 9 (E encodeMint _mint))
      !> encodeKeyedStrictMaybe 11 _scriptIntegrityHash
      !> encodeKeyedStrictMaybe 7 _adHash
      !> encodeKeyedStrictMaybe 15 _txnetworkid

instance
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (PParamsUpdate era),
    Show (Value era)
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
        ofield
          (\x tx -> tx {_vldt = (_vldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        field
          (\x tx -> tx {_certs = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 = field (\x tx -> tx {_wdrls = x}) From
      bodyFields 6 = ofield (\x tx -> tx {_update = x}) From
      bodyFields 7 = ofield (\x tx -> tx {_adHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {_vldt = (_vldt tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {_mint = x}) (D decodeMint)
      bodyFields 11 = ofield (\x tx -> tx {_scriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {_reqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = ofield (\x tx -> tx {_txnetworkid = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

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

instance
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (PParamsUpdate era),
    Show (Value era)
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

txBodyRawEq ::
  ( Era era,
    Eq (Core.TxOut era),
    Compactible (Value era),
    Eq (Value era),
    Eq (PParamsUpdate era),
    Eq (PParamsHKD StrictMaybe era)
  ) =>
  AlonzoTxBody era ->
  AlonzoTxBody era ->
  Bool
txBodyRawEq (TxBodyConstr x) (TxBodyConstr y) = contentsEq x y
