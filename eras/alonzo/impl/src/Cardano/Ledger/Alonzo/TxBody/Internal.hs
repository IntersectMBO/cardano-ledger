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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides Alonzo TxBody internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Alonzo.TxBody.Internal (
  AlonzoTxOut (..),
  AlonzoEraTxOut (..),
  -- Constructors are not exported for safety:
  Addr28Extra,
  DataHash32,
  AlonzoTxBody (
    AlonzoTxBody,
    atbInputs,
    atbCollateral,
    atbOutputs,
    atbCerts,
    atbWithdrawals,
    atbTxFee,
    atbValidityInterval,
    atbUpdate,
    atbReqSignerHashes,
    atbMint,
    atbScriptIntegrityHash,
    atbAuxDataHash,
    atbTxNetworkId
  ),
  AlonzoTxBodyRaw,
  AlonzoTxBodyUpgradeError (..),
  AlonzoEraTxBody (..),
  ShelleyEraTxBody (..),
  AllegraEraTxBody (..),
  MaryEraTxBody (..),
  Indexable (..),
  inputs',
  collateral',
  outputs',
  certs',
  withdrawals',
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
  alonzoRedeemerPointer,
  alonzoRedeemerPointerInverse,
)
where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.PParams ()
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AsItem (..),
  AsIx (..),
  AsIxItem (..),
  PlutusPurpose,
 )
import Cardano.Ledger.Alonzo.TxAuxData (AuxiliaryDataHash (..))
import Cardano.Ledger.Alonzo.TxCert ()
import Cardano.Ledger.Alonzo.TxOut
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..))
import Cardano.Ledger.Mary.Value (
  MaryValue (MaryValue),
  MultiAsset (..),
  PolicyID (..),
  policies,
 )
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeHash, SafeToHash)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (..), Update (..))
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (isSJust)
import Data.OSet.Strict (OSet)
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Void (absurd)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

type ScriptIntegrityHash c = SafeHash c EraIndependentScriptIntegrity

class (MaryEraTxBody era, AlonzoEraTxOut era) => AlonzoEraTxBody era where
  collateralInputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  reqSignerHashesTxBodyL :: Lens' (TxBody era) (Set (KeyHash 'Witness (EraCrypto era)))

  scriptIntegrityHashTxBodyL ::
    Lens' (TxBody era) (StrictMaybe (ScriptIntegrityHash (EraCrypto era)))

  networkIdTxBodyL :: Lens' (TxBody era) (StrictMaybe Network)

  -- | This function is called @rdptr@ in the spec. Given a `TxBody` and a plutus
  -- purpose with an item, we should be able to find the plutus purpose as in index
  redeemerPointer ::
    TxBody era ->
    PlutusPurpose AsItem era ->
    StrictMaybe (PlutusPurpose AsIx era)

  -- | This is an inverse of `redeemerPointer`. Given purpose as an index return it as an item.
  redeemerPointerInverse ::
    TxBody era ->
    PlutusPurpose AsIx era ->
    StrictMaybe (PlutusPurpose AsIxItem era)

-- ======================================

data AlonzoTxBodyRaw era = AlonzoTxBodyRaw
  { atbrInputs :: !(Set (TxIn (EraCrypto era)))
  , atbrCollateral :: !(Set (TxIn (EraCrypto era)))
  , atbrOutputs :: !(StrictSeq (TxOut era))
  , atbrCerts :: !(StrictSeq (TxCert era))
  , atbrWithdrawals :: !(Withdrawals (EraCrypto era))
  , atbrTxFee :: !Coin
  , atbrValidityInterval :: !ValidityInterval
  , atbrUpdate :: !(StrictMaybe (Update era))
  , atbrReqSignerHashes :: Set (KeyHash 'Witness (EraCrypto era))
  , atbrMint :: !(MultiAsset (EraCrypto era))
  , atbrScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (EraCrypto era)))
  , atbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  , atbrTxNetworkId :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (AlonzoTxBodyRaw era)

instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (AlonzoTxBodyRaw era)

instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
  NFData (AlonzoTxBodyRaw era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (AlonzoTxBodyRaw era)

newtype AlonzoTxBody era = TxBodyConstr (MemoBytes AlonzoTxBodyRaw era)
  deriving (ToCBOR, Generic)
  deriving newtype (SafeToHash)

instance Memoized AlonzoTxBody where
  type RawType AlonzoTxBody = AlonzoTxBodyRaw

data AlonzoTxBodyUpgradeError
  = -- | The TxBody contains a protocol parameter update that attempts to update
    -- the min UTxO. Since this doesn't exist in Alonzo, we fail if an attempt is
    -- made to update it.
    ATBUEMinUTxOUpdated
  deriving (Show)

instance Crypto c => EraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance EraTxBody (AlonzoEra StandardCrypto) #-}

  type TxBody (AlonzoEra c) = AlonzoTxBody (AlonzoEra c)
  type TxBodyUpgradeError (AlonzoEra c) = AlonzoTxBodyUpgradeError

  mkBasicTxBody = mkMemoized emptyAlonzoTxBodyRaw

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
    lensMemoRawType
      atbrAuxDataHash
      (\txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash})
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = allInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF =
    to $ \txBody -> (txBody ^. inputsTxBodyL) `Set.union` (txBody ^. collateralInputsTxBodyL)
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType
      atbrWithdrawals
      (\txBodyRaw withdrawals_ -> txBodyRaw {atbrWithdrawals = withdrawals_})
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType atbrCerts (\txBodyRaw certs_ -> txBodyRaw {atbrCerts = certs_})
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

  upgradeTxBody
    MaryTxBody
      { mtbInputs
      , mtbOutputs
      , mtbCerts
      , mtbWithdrawals
      , mtbTxFee
      , mtbValidityInterval
      , mtbUpdate
      , mtbAuxDataHash
      , mtbMint
      } = do
      certs <-
        traverse
          (left absurd . upgradeTxCert)
          mtbCerts

      updates <- traverse upgradeUpdate mtbUpdate
      pure $
        AlonzoTxBody
          { atbInputs = mtbInputs
          , atbOutputs = upgradeTxOut <$> mtbOutputs
          , atbCerts = certs
          , atbWithdrawals = mtbWithdrawals
          , atbTxFee = mtbTxFee
          , atbValidityInterval = mtbValidityInterval
          , atbUpdate = updates
          , atbAuxDataHash = mtbAuxDataHash
          , atbMint = mtbMint
          , atbCollateral = mempty
          , atbReqSignerHashes = mempty
          , atbScriptIntegrityHash = SNothing
          , atbTxNetworkId = SNothing
          }
      where
        upgradeUpdate ::
          Update (MaryEra c) ->
          Either AlonzoTxBodyUpgradeError (Update (AlonzoEra c))
        upgradeUpdate (Update pp epoch) =
          Update <$> upgradeProposedPPUpdates pp <*> pure epoch

        upgradeProposedPPUpdates ::
          ProposedPPUpdates (MaryEra c) ->
          Either AlonzoTxBodyUpgradeError (ProposedPPUpdates (AlonzoEra c))
        upgradeProposedPPUpdates (ProposedPPUpdates m) =
          ProposedPPUpdates
            <$> traverse
              ( \ppu -> do
                  when (isSJust $ ppu ^. ppuMinUTxOValueL) $
                    Left ATBUEMinUTxOUpdated
                  pure $ upgradePParamsUpdate def ppu
              )
              m

instance Crypto c => ShelleyEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (AlonzoEra StandardCrypto) #-}

  ttlTxBodyL = notSupportedInThisEraL

  updateTxBodyL =
    lensMemoRawType atbrUpdate (\txBodyRaw update_ -> txBodyRaw {atbrUpdate = update_})
  {-# INLINEABLE updateTxBodyL #-}

instance Crypto c => AllegraEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (AlonzoEra StandardCrypto) #-}

  vldtTxBodyL =
    lensMemoRawType atbrValidityInterval (\txBodyRaw vldt_ -> txBodyRaw {atbrValidityInterval = vldt_})
  {-# INLINEABLE vldtTxBodyL #-}

instance Crypto c => MaryEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance MaryEraTxBody (AlonzoEra StandardCrypto) #-}

  mintTxBodyL =
    lensMemoRawType atbrMint (\txBodyRaw mint_ -> txBodyRaw {atbrMint = mint_})
  {-# INLINEABLE mintTxBodyL #-}

  mintValueTxBodyF = mintTxBodyL . to (MaryValue mempty)
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF = to (policies . atbrMint . getMemoRawType)
  {-# INLINEABLE mintedTxBodyF #-}

instance Crypto c => AlonzoEraTxBody (AlonzoEra c) where
  {-# SPECIALIZE instance AlonzoEraTxBody (AlonzoEra StandardCrypto) #-}

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

  redeemerPointer = alonzoRedeemerPointer

  redeemerPointerInverse = alonzoRedeemerPointerInverse

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (AlonzoTxBody era)

deriving instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (AlonzoTxBody era)

deriving instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
  NFData (AlonzoTxBody era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (AlonzoTxBody era)

deriving via
  (Mem AlonzoTxBodyRaw era)
  instance
    (Era era, DecCBOR (TxOut era), DecCBOR (TxCert era), DecCBOR (PParamsUpdate era)) =>
    DecCBOR (Annotator (AlonzoTxBody era))

pattern AlonzoTxBody ::
  (EraTxOut era, EraTxCert era) =>
  Set (TxIn (EraCrypto era)) ->
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (TxCert era) ->
  Withdrawals (EraCrypto era) ->
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
  { atbInputs
  , atbCollateral
  , atbOutputs
  , atbCerts
  , atbWithdrawals
  , atbTxFee
  , atbValidityInterval
  , atbUpdate
  , atbReqSignerHashes
  , atbMint
  , atbScriptIntegrityHash
  , atbAuxDataHash
  , atbTxNetworkId
  } <-
  ( getMemoRawType ->
      AlonzoTxBodyRaw
        { atbrInputs = atbInputs
        , atbrCollateral = atbCollateral
        , atbrOutputs = atbOutputs
        , atbrCerts = atbCerts
        , atbrWithdrawals = atbWithdrawals
        , atbrTxFee = atbTxFee
        , atbrValidityInterval = atbValidityInterval
        , atbrUpdate = atbUpdate
        , atbrReqSignerHashes = atbReqSignerHashes
        , atbrMint = atbMint
        , atbrScriptIntegrityHash = atbScriptIntegrityHash
        , atbrAuxDataHash = atbAuxDataHash
        , atbrTxNetworkId = atbTxNetworkId
        }
    )
  where
    AlonzoTxBody
      inputs
      collateral
      outputs
      certs
      withdrawals
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
            { atbrInputs = inputs
            , atbrCollateral = collateral
            , atbrOutputs = outputs
            , atbrCerts = certs
            , atbrWithdrawals = withdrawals
            , atbrTxFee = txFee
            , atbrValidityInterval = validityInterval
            , atbrUpdate = update
            , atbrReqSignerHashes = reqSignerHashes
            , atbrMint = mint
            , atbrScriptIntegrityHash = scriptIntegrityHash
            , atbrAuxDataHash = auxDataHash
            , atbrTxNetworkId = txNetworkId
            }

{-# COMPLETE AlonzoTxBody #-}

type instance MemoHashIndex AlonzoTxBodyRaw = EraIndependentTxBody

instance c ~ EraCrypto era => HashAnnotated (AlonzoTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (AlonzoBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

inputs' :: AlonzoTxBody era -> Set (TxIn (EraCrypto era))
collateral' :: AlonzoTxBody era -> Set (TxIn (EraCrypto era))
outputs' :: AlonzoTxBody era -> StrictSeq (TxOut era)
certs' :: AlonzoTxBody era -> StrictSeq (TxCert era)
txfee' :: AlonzoTxBody era -> Coin
withdrawals' :: AlonzoTxBody era -> Withdrawals (EraCrypto era)
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

withdrawals' = atbrWithdrawals . getMemoRawType

txfee' = atbrTxFee . getMemoRawType

vldt' = atbrValidityInterval . getMemoRawType

update' = atbrUpdate . getMemoRawType

reqSignerHashes' = atbrReqSignerHashes . getMemoRawType

adHash' = atbrAuxDataHash . getMemoRawType

mint' = atbrMint . getMemoRawType

scriptIntegrityHash' = atbrScriptIntegrityHash . getMemoRawType

txnetworkid' = atbrTxNetworkId . getMemoRawType

instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (AlonzoTxBody era)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AlonzoTxBody era)

instance
  (Era era, EncCBOR (TxOut era), EncCBOR (TxCert era), EncCBOR (PParamsUpdate era)) =>
  EncCBOR (AlonzoTxBodyRaw era)
  where
  encCBOR
    AlonzoTxBodyRaw
      { atbrInputs
      , atbrCollateral
      , atbrOutputs
      , atbrCerts
      , atbrWithdrawals
      , atbrTxFee
      , atbrValidityInterval = ValidityInterval bot top
      , atbrUpdate
      , atbrReqSignerHashes
      , atbrMint
      , atbrScriptIntegrityHash
      , atbrAuxDataHash
      , atbrTxNetworkId
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
          !> Omit (null . unWithdrawals) (Key 5 (To atbrWithdrawals))
          !> encodeKeyedStrictMaybe 6 atbrUpdate
          !> encodeKeyedStrictMaybe 8 bot
          !> Omit null (Key 14 (To atbrReqSignerHashes))
          !> Omit (== mempty) (Key 9 (To atbrMint))
          !> encodeKeyedStrictMaybe 11 atbrScriptIntegrityHash
          !> encodeKeyedStrictMaybe 7 atbrAuxDataHash
          !> encodeKeyedStrictMaybe 15 atbrTxNetworkId

instance
  (Era era, DecCBOR (TxOut era), DecCBOR (TxCert era), DecCBOR (PParamsUpdate era)) =>
  DecCBOR (AlonzoTxBodyRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "AlonzoTxBodyRaw"
        emptyAlonzoTxBodyRaw
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
      bodyFields 5 = field (\x tx -> tx {atbrWithdrawals = x}) From
      bodyFields 6 = ofield (\x tx -> tx {atbrUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {atbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {atbrValidityInterval = (atbrValidityInterval tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {atbrMint = x}) From
      bodyFields 11 = ofield (\x tx -> tx {atbrScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {atbrReqSignerHashes = x}) From
      bodyFields 15 = ofield (\x tx -> tx {atbrTxNetworkId = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]

emptyAlonzoTxBodyRaw :: AlonzoTxBodyRaw era
emptyAlonzoTxBodyRaw =
  AlonzoTxBodyRaw
    mempty
    mempty
    StrictSeq.empty
    StrictSeq.empty
    (Withdrawals mempty)
    mempty
    (ValidityInterval SNothing SNothing)
    SNothing
    mempty
    mempty
    SNothing
    SNothing
    SNothing

instance
  (Era era, DecCBOR (TxOut era), DecCBOR (TxCert era), DecCBOR (PParamsUpdate era)) =>
  DecCBOR (Annotator (AlonzoTxBodyRaw era))
  where
  decCBOR = pure <$> decCBOR

alonzoRedeemerPointer ::
  forall era.
  MaryEraTxBody era =>
  TxBody era ->
  AlonzoPlutusPurpose AsItem era ->
  StrictMaybe (AlonzoPlutusPurpose AsIx era)
alonzoRedeemerPointer txBody = \case
  AlonzoSpending txIn ->
    AlonzoSpending <$> indexOf txIn (txBody ^. inputsTxBodyL)
  AlonzoMinting policyID ->
    AlonzoMinting <$> indexOf policyID (txBody ^. mintedTxBodyF :: Set (PolicyID (EraCrypto era)))
  AlonzoCertifying txCert ->
    AlonzoCertifying <$> indexOf txCert (txBody ^. certsTxBodyL)
  AlonzoRewarding rewardAccount ->
    AlonzoRewarding <$> indexOf rewardAccount (unWithdrawals (txBody ^. withdrawalsTxBodyL))

alonzoRedeemerPointerInverse ::
  MaryEraTxBody era =>
  TxBody era ->
  AlonzoPlutusPurpose AsIx era ->
  StrictMaybe (AlonzoPlutusPurpose AsIxItem era)
alonzoRedeemerPointerInverse txBody = \case
  AlonzoSpending idx ->
    AlonzoSpending <$> fromIndex idx (txBody ^. inputsTxBodyL)
  AlonzoMinting idx ->
    AlonzoMinting <$> fromIndex idx (txBody ^. mintedTxBodyF)
  AlonzoCertifying idx ->
    AlonzoCertifying <$> fromIndex idx (txBody ^. certsTxBodyL)
  AlonzoRewarding idx ->
    AlonzoRewarding <$> fromIndex idx (unWithdrawals (txBody ^. withdrawalsTxBodyL))

class Indexable elem container where
  indexOf :: AsItem Word32 elem -> container -> StrictMaybe (AsIx Word32 elem)
  fromIndex :: AsIx Word32 elem -> container -> StrictMaybe (AsIxItem Word32 elem)

instance Ord k => Indexable k (Set k) where
  indexOf (AsItem n) s = case Set.lookupIndex n s of
    Just x -> SJust (AsIx (fromIntegral @Int @Word32 x))
    Nothing -> SNothing
  fromIndex (AsIx w32) s =
    let i = fromIntegral @Word32 @Int w32
     in if i < Set.size s
          then SJust $ AsIxItem w32 (Set.elemAt i s)
          else SNothing

instance Eq k => Indexable k (StrictSeq k) where
  indexOf (AsItem n) seqx = case StrictSeq.findIndexL (== n) seqx of
    Just m -> SJust (AsIx (fromIntegral @Int @Word32 m))
    Nothing -> SNothing
  fromIndex (AsIx w32) seqx =
    case StrictSeq.lookup (fromIntegral @Word32 @Int w32) seqx of
      Nothing -> SNothing
      Just x -> SJust $ AsIxItem w32 x

instance Ord k => Indexable k (Map.Map k v) where
  indexOf (AsItem n) mp = case Map.lookupIndex n mp of
    Just x -> SJust (AsIx (fromIntegral @Int @Word32 x))
    Nothing -> SNothing
  fromIndex (AsIx w32) mp =
    let i = fromIntegral @Word32 @Int w32
     in if i < fromIntegral (Map.size mp)
          then SJust . AsIxItem w32 . fst $ Map.elemAt i mp
          else SNothing

instance Ord k => Indexable k (OSet k) where
  indexOf asItem = indexOf asItem . OSet.toStrictSeq
  fromIndex asIndex = fromIndex asIndex . OSet.toStrictSeq
