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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxBody
  ( AlonzoTxOut (..),
    AlonzoEraTxOut (..),
    -- Constructors are not exported for safety:
    Addr28Extra,
    DataHash32,
    AlonzoTxBody
      ( AlonzoTxBody,
        atbInputs,
        atbCollateral,
        atbOutputs,
        atbCerts,
        atbWdrls,
        atbTxFee,
        atbValidityInterval,
        atbUpdate,
        atbReqSignerHashes,
        atbMint,
        atbScriptIntegrityHash,
        atbAuxDataHash,
        atbTxNetworkId
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

    -- * Deprecated
    TxOut,
    TxBody,
  )
where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxBody (..), ScriptIntegrityHash)
import Cardano.Ledger.Alonzo.Data (AuxiliaryDataHash (..))
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Scripts ()
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Cardano.Ledger.Binary
  ( Annotator,
    FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core hiding (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (MaryValue (MaryValue), MultiAsset (..), policies, policyID)
import Cardano.Ledger.MemoBytes
  ( Mem,
    MemoBytes,
    MemoHashIndex,
    Memoized (..),
    getMemoRawType,
    getMemoSafeHash,
    lensMemoRawType,
    mkMemoized,
  )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..), decodeMint, encodeMint)
import Control.DeepSeq (NFData (..))
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

data AlonzoTxBodyRaw era = AlonzoTxBodyRaw
  { atbrInputs :: !(Set (TxIn (EraCrypto era))),
    atbrCollateral :: !(Set (TxIn (EraCrypto era))),
    atbrOutputs :: !(StrictSeq (Core.TxOut era)),
    atbrCerts :: !(StrictSeq (DCert (EraCrypto era))),
    atbrWdrls :: !(Wdrl (EraCrypto era)),
    atbrTxFee :: !Coin,
    atbrValidityInterval :: !ValidityInterval,
    atbrUpdate :: !(StrictMaybe (Update era)),
    atbrReqSignerHashes :: Set (KeyHash 'Witness (EraCrypto era)),
    atbrMint :: !(MultiAsset (EraCrypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.MaryValue, not a Cardano.Ledger.Core.Value.
    -- Operations on the TxBody in the AlonzoEra depend upon this.
    -- We now store only the MultiAsset part of a Mary.Value.
    atbrScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era))),
    atbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era))),
    atbrTxNetworkId :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  (Era era, Eq (Core.TxOut era), Eq (PParamsUpdate era), Eq (Value era), Compactible (Value era)) =>
  Eq (AlonzoTxBodyRaw era)

instance (Era era, NoThunks (Core.TxOut era), NoThunks (PParamsUpdate era)) => NoThunks (AlonzoTxBodyRaw era)

instance (Era era, NFData (Core.TxOut era), NFData (PParamsUpdate era)) => NFData (AlonzoTxBodyRaw era)

deriving instance
  (Era era, Show (Core.TxOut era), Show (PParamsUpdate era), Show (Value era), Val (Value era)) =>
  Show (AlonzoTxBodyRaw era)

newtype AlonzoTxBody era = TxBodyConstr (MemoBytes AlonzoTxBodyRaw era)
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

instance Memoized AlonzoTxBody where
  type RawType AlonzoTxBody = AlonzoTxBodyRaw

type TxBody era = AlonzoTxBody era

{-# DEPRECATED TxBody "Use `AlonzoTxBody` instead" #-}

instance CC.Crypto c => EraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance EraTxBody (AlonzoEra CC.StandardCrypto) #-}

  type TxBody (AlonzoEra c) = AlonzoTxBody (AlonzoEra c)

  mkBasicTxBody = mkMemoized basicAlonzoTxBodyRaw

  inputsTxBodyL =
    lensMemoRawType atbrInputs (\txBodyRaw inputs_ -> txBodyRaw {atbrInputs = inputs_})
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType atbrOutputs (\txBodyRaw outputs_ -> txBodyRaw {atbrOutputs = outputs_})
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType atbrTxFee (\txBodyRaw fee_ -> txBodyRaw {atbrTxFee = fee_})
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType atbrAuxDataHash (\txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash})
  {-# INLINEABLE auxDataHashTxBodyL #-}

  allInputsTxBodyF =
    to $ \txBody -> (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL)
  {-# INLINEABLE allInputsTxBodyF #-}

instance CC.Crypto c => ShelleyEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  wdrlsTxBodyL =
    lensMemoRawType atbrWdrls (\txBodyRaw wdrls_ -> txBodyRaw {atbrWdrls = wdrls_})
  {-# INLINEABLE wdrlsTxBodyL #-}

  ttlTxBodyL = notSupportedInThisEraL

  updateTxBodyL =
    lensMemoRawType atbrUpdate (\txBodyRaw update_ -> txBodyRaw {atbrUpdate = update_})
  {-# INLINEABLE updateTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType atbrCerts (\txBodyRaw certs_ -> txBodyRaw {atbrCerts = certs_})
  {-# INLINEABLE certsTxBodyL #-}

instance CC.Crypto c => ShelleyMAEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyMAEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  vldtTxBodyL =
    lensMemoRawType atbrValidityInterval (\txBodyRaw vldt_ -> txBodyRaw {atbrValidityInterval = vldt_})
  {-# INLINEABLE vldtTxBodyL #-}

  mintTxBodyL =
    lensMemoRawType atbrMint (\txBodyRaw mint_ -> txBodyRaw {atbrMint = mint_})
  {-# INLINEABLE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue 0)
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF = to (Set.map policyID . policies . atbrMint . getMemoRawType)
  {-# INLINEABLE mintedTxBodyF #-}

instance CC.Crypto c => AlonzoEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (AlonzoEra CC.StandardCrypto) #-}

  collateralInputsTxBodyL =
    lensMemoRawType atbrCollateral (\txBodyRaw collateral_ -> txBodyRaw {atbrCollateral = collateral_})
  {-# INLINEABLE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensMemoRawType
      atbrReqSignerHashes
      (\txBodyRaw reqSignerHashes_ -> txBodyRaw {atbrReqSignerHashes = reqSignerHashes_})
  {-# INLINEABLE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType
      atbrScriptIntegrityHash
      (\txBodyRaw scriptIntegrityHash_ -> txBodyRaw {atbrScriptIntegrityHash = scriptIntegrityHash_})
  {-# INLINEABLE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL =
    lensMemoRawType atbrTxNetworkId (\txBodyRaw networkId -> txBodyRaw {atbrTxNetworkId = networkId})
  {-# INLINEABLE networkIdTxBodyL #-}

deriving newtype instance
  (Era era, Eq (Core.TxOut era), Eq (PParamsUpdate era), Eq (Value era), Compactible (Value era)) =>
  Eq (AlonzoTxBody era)

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
  (Mem AlonzoTxBodyRaw era)
  instance
    ( Era era,
      FromCBOR (Core.TxOut era),
      FromCBOR (PParamsUpdate era),
      Show (Value era)
    ) =>
    FromCBOR (Annotator (AlonzoTxBody era))

pattern AlonzoTxBody ::
  EraTxOut era =>
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
  { atbInputs,
    atbCollateral,
    atbOutputs,
    atbCerts,
    atbWdrls,
    atbTxFee,
    atbValidityInterval,
    atbUpdate,
    atbReqSignerHashes,
    atbMint,
    atbScriptIntegrityHash,
    atbAuxDataHash,
    atbTxNetworkId
  } <-
  ( getMemoRawType ->
      AlonzoTxBodyRaw
        { atbrInputs = atbInputs,
          atbrCollateral = atbCollateral,
          atbrOutputs = atbOutputs,
          atbrCerts = atbCerts,
          atbrWdrls = atbWdrls,
          atbrTxFee = atbTxFee,
          atbrValidityInterval = atbValidityInterval,
          atbrUpdate = atbUpdate,
          atbrReqSignerHashes = atbReqSignerHashes,
          atbrMint = atbMint,
          atbrScriptIntegrityHash = atbScriptIntegrityHash,
          atbrAuxDataHash = atbAuxDataHash,
          atbrTxNetworkId = atbTxNetworkId
        }
    )
  where
    AlonzoTxBody
      inputs
      collateral
      outputs
      certs
      wdrls
      txFee
      validityInterval
      update
      reqSignerHashes
      mint
      scriptIntegrityHash
      auxDataHash
      txNetworkId =
        mkMemoized $
          AlonzoTxBodyRaw
            { atbrInputs = inputs,
              atbrCollateral = collateral,
              atbrOutputs = outputs,
              atbrCerts = certs,
              atbrWdrls = wdrls,
              atbrTxFee = txFee,
              atbrValidityInterval = validityInterval,
              atbrUpdate = update,
              atbrReqSignerHashes = reqSignerHashes,
              atbrMint = mint,
              atbrScriptIntegrityHash = scriptIntegrityHash,
              atbrAuxDataHash = auxDataHash,
              atbrTxNetworkId = txNetworkId
            }

{-# COMPLETE AlonzoTxBody #-}

type instance MemoHashIndex AlonzoTxBodyRaw = EraIndependentTxBody

instance (c ~ EraCrypto era) => HashAnnotated (AlonzoTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: AlonzoTxBody era -> Set (TxIn (EraCrypto era))
collateral' :: AlonzoTxBody era -> Set (TxIn (EraCrypto era))
outputs' :: AlonzoTxBody era -> StrictSeq (Core.TxOut era)
certs' :: AlonzoTxBody era -> StrictSeq (DCert (EraCrypto era))
txfee' :: AlonzoTxBody era -> Coin
wdrls' :: AlonzoTxBody era -> Wdrl (EraCrypto era)
vldt' :: AlonzoTxBody era -> ValidityInterval
update' :: AlonzoTxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: AlonzoTxBody era -> Set (KeyHash 'Witness (EraCrypto era))
adHash' :: AlonzoTxBody era -> StrictMaybe (AuxiliaryDataHash (EraCrypto era))
mint' :: AlonzoTxBody era -> MultiAsset (EraCrypto era)
scriptIntegrityHash' :: AlonzoTxBody era -> StrictMaybe (ScriptIntegrityHash (EraCrypto era))
txnetworkid' :: AlonzoTxBody era -> StrictMaybe Network
inputs' = atbrInputs . getMemoRawType

collateral' = atbrCollateral . getMemoRawType

outputs' = atbrOutputs . getMemoRawType

certs' = atbrCerts . getMemoRawType

wdrls' = atbrWdrls . getMemoRawType

txfee' = atbrTxFee . getMemoRawType

vldt' = atbrValidityInterval . getMemoRawType

update' = atbrUpdate . getMemoRawType

reqSignerHashes' = atbrReqSignerHashes . getMemoRawType

adHash' = atbrAuxDataHash . getMemoRawType

mint' = atbrMint . getMemoRawType

scriptIntegrityHash' = atbrScriptIntegrityHash . getMemoRawType

txnetworkid' = atbrTxNetworkId . getMemoRawType

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

instance
  (Era era, ToCBOR (Core.TxOut era), ToCBOR (PParamsUpdate era)) =>
  ToCBOR (AlonzoTxBodyRaw era)
  where
  toCBOR
    AlonzoTxBodyRaw
      { atbrInputs,
        atbrCollateral,
        atbrOutputs,
        atbrCerts,
        atbrWdrls,
        atbrTxFee,
        atbrValidityInterval = ValidityInterval bot top,
        atbrUpdate,
        atbrReqSignerHashes,
        atbrMint,
        atbrScriptIntegrityHash,
        atbrAuxDataHash,
        atbrTxNetworkId
      } =
      encode $
        Keyed
          ( \i ifee o f t c w u b rsh mi sh ah ni ->
              AlonzoTxBodyRaw i ifee o c w f (ValidityInterval b t) u rsh mi sh ah ni
          )
          !> Key 0 (To atbrInputs)
          !> Omit null (Key 13 (To atbrCollateral))
          !> Key 1 (To atbrOutputs)
          !> Key 2 (To atbrTxFee)
          !> encodeKeyedStrictMaybe 3 top
          !> Omit null (Key 4 (To atbrCerts))
          !> Omit (null . unWdrl) (Key 5 (To atbrWdrls))
          !> encodeKeyedStrictMaybe 6 atbrUpdate
          !> encodeKeyedStrictMaybe 8 bot
          !> Omit null (Key 14 (To atbrReqSignerHashes))
          !> Omit (== mempty) (Key 9 (E encodeMint atbrMint))
          !> encodeKeyedStrictMaybe 11 atbrScriptIntegrityHash
          !> encodeKeyedStrictMaybe 7 atbrAuxDataHash
          !> encodeKeyedStrictMaybe 15 atbrTxNetworkId

instance
  ( Era era,
    FromCBOR (Core.TxOut era),
    FromCBOR (PParamsUpdate era),
    Show (Value era)
  ) =>
  FromCBOR (AlonzoTxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "AlonzoTxBodyRaw"
        basicAlonzoTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (AlonzoTxBodyRaw era)
      bodyFields 0 = field (\x tx -> tx {atbrInputs = x}) From
      bodyFields 13 = field (\x tx -> tx {atbrCollateral = x}) From
      bodyFields 1 = field (\x tx -> tx {atbrOutputs = x}) From
      bodyFields 2 = field (\x tx -> tx {atbrTxFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {atbrValidityInterval = (atbrValidityInterval tx) {invalidHereafter = x}})
          From
      bodyFields 4 = field (\x tx -> tx {atbrCerts = x}) From
      bodyFields 5 = field (\x tx -> tx {atbrWdrls = x}) From
      bodyFields 6 = ofield (\x tx -> tx {atbrUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {atbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {atbrValidityInterval = (atbrValidityInterval tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {atbrMint = x}) (D decodeMint)
      bodyFields 11 = ofield (\x tx -> tx {atbrScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {atbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {atbrTxNetworkId = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

basicAlonzoTxBodyRaw :: AlonzoTxBodyRaw era
basicAlonzoTxBodyRaw =
  AlonzoTxBodyRaw
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
  FromCBOR (Annotator (AlonzoTxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR
