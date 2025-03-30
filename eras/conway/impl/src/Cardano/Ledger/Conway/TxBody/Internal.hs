{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides Conway TxBody internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Conway.TxBody.Internal (
  ConwayEraTxBody (..),
  ConwayTxBody (
    MkConwayTxBody,
    ConwayTxBody,
    ctbSpendInputs,
    ctbCollateralInputs,
    ctbReferenceInputs,
    ctbOutputs,
    ctbCollateralReturn,
    ctbTotalCollateral,
    ctbCerts,
    ctbWithdrawals,
    ctbTxfee,
    ctbVldt,
    ctbReqSignerHashes,
    ctbMint,
    ctbScriptIntegrityHash,
    ctbAdHash,
    ctbTxNetworkId,
    ctbVotingProcedures,
    ctbProposalProcedures,
    ctbCurrentTreasuryValue,
    ctbTreasuryDonation
  ),
  ConwayTxBodyRaw (..),
  conwayTotalDepositsTxBody,
  conwayProposalsDeposits,
) where

import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (
  BabbageTxBody (..),
  allSizedOutputsBabbageTxBodyF,
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
 )
import Cardano.Ledger.BaseTypes (Network, fromSMaybe, isSJust)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Sized (..),
  ToCBOR (..),
  mkSized,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Field (..),
  Wrapped (..),
  decode,
  encode,
  encodeKeyedStrictMaybe,
  field,
  fieldGuarded,
  invalidField,
  ofield,
  (!>),
 )
import Cardano.Ledger.Coin (Coin (..), decodePositiveCoin)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Governance.Procedures (ProposalProcedure, VotingProcedures (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppGovActionDepositL)
import Cardano.Ledger.Conway.Scripts (ConwayEraScript, ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert,
  ConwayTxCertUpgradeError,
 )
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Mary.Value (MultiAsset (..), policies)
import Cardano.Ledger.MemoBytes (
  EqRaw,
  MemoBytes (..),
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Control.Arrow (left)
import Control.DeepSeq (NFData)
import Control.Monad (unless, when)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', to, (^.))
import NoThunks.Class (NoThunks)

instance Memoized (ConwayTxBody era) where
  type RawType (ConwayTxBody era) = ConwayTxBodyRaw era

data ConwayTxBodyRaw era = ConwayTxBodyRaw
  { ctbrSpendInputs :: !(Set TxIn)
  , ctbrCollateralInputs :: !(Set TxIn)
  , ctbrReferenceInputs :: !(Set TxIn)
  , ctbrOutputs :: !(StrictSeq (Sized (TxOut era)))
  , ctbrCollateralReturn :: !(StrictMaybe (Sized (TxOut era)))
  , ctbrTotalCollateral :: !(StrictMaybe Coin)
  , ctbrCerts :: !(OSet.OSet (TxCert era))
  , ctbrWithdrawals :: !Withdrawals
  , ctbrFee :: !Coin
  , ctbrVldt :: !ValidityInterval
  , ctbrReqSignerHashes :: !(Set (KeyHash 'Witness))
  , ctbrMint :: !MultiAsset
  , ctbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
  , ctbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , ctbrNetworkId :: !(StrictMaybe Network)
  , ctbrVotingProcedures :: !(VotingProcedures era)
  , ctbrProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
  , ctbrCurrentTreasuryValue :: !(StrictMaybe Coin)
  , ctbrTreasuryDonation :: !Coin
  }
  deriving (Generic, Typeable)

deriving instance (EraPParams era, Eq (TxCert era), Eq (TxOut era)) => Eq (ConwayTxBodyRaw era)

instance
  (EraPParams era, NoThunks (TxCert era), NoThunks (TxOut era)) =>
  NoThunks (ConwayTxBodyRaw era)

instance
  (EraPParams era, NFData (TxCert era), NFData (TxOut era)) =>
  NFData (ConwayTxBodyRaw era)

deriving instance
  (EraPParams era, Show (TxCert era), Show (TxOut era)) =>
  Show (ConwayTxBodyRaw era)

instance
  ( EraPParams era
  , EraTxCert era
  , DecCBOR (TxOut era)
  ) =>
  DecCBOR (ConwayTxBodyRaw era)
  where
  decCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        basicConwayTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (ConwayTxBodyRaw era)
      bodyFields 0 = field (\x tx -> tx {ctbrSpendInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {ctbrOutputs = x}) From
      bodyFields 2 = field (\x tx -> tx {ctbrFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {ctbrVldt = (ctbrVldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        fieldGuarded
          (emptyFailure "Certificates" "non-empty")
          OSet.null
          (\x tx -> tx {ctbrCerts = x})
          From
      bodyFields 5 =
        fieldGuarded
          (emptyFailure "Withdrawals" "non-empty")
          (null . unWithdrawals)
          (\x tx -> tx {ctbrWithdrawals = x})
          From
      bodyFields 7 = ofield (\x tx -> tx {ctbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {ctbrVldt = (ctbrVldt tx) {invalidBefore = x}})
          From
      bodyFields 9 =
        fieldGuarded
          (emptyFailure "Mint" "non-empty")
          (== mempty)
          (\x tx -> tx {ctbrMint = x})
          From
      bodyFields 11 = ofield (\x tx -> tx {ctbrScriptIntegrityHash = x}) From
      bodyFields 13 =
        fieldGuarded
          (emptyFailure "Collateral Inputs" "non-empty")
          null
          (\x tx -> tx {ctbrCollateralInputs = x})
          From
      bodyFields 14 =
        fieldGuarded
          (emptyFailure "Required Signer Hashes" "non-empty")
          null
          (\x tx -> tx {ctbrReqSignerHashes = x})
          From
      bodyFields 15 = ofield (\x tx -> tx {ctbrNetworkId = x}) From
      bodyFields 16 = ofield (\x tx -> tx {ctbrCollateralReturn = x}) From
      bodyFields 17 = ofield (\x tx -> tx {ctbrTotalCollateral = x}) From
      bodyFields 18 =
        fieldGuarded
          (emptyFailure "Reference Inputs" "non-empty")
          null
          (\x tx -> tx {ctbrReferenceInputs = x})
          From
      bodyFields 19 =
        fieldGuarded
          (emptyFailure "VotingProcedures" "non-empty")
          (null . unVotingProcedures)
          (\x tx -> tx {ctbrVotingProcedures = x})
          From
      bodyFields 20 =
        fieldGuarded
          (emptyFailure "ProposalProcedures" "non-empty")
          OSet.null
          (\x tx -> tx {ctbrProposalProcedures = x})
          From
      bodyFields 21 = ofield (\x tx -> tx {ctbrCurrentTreasuryValue = x}) From
      bodyFields 22 =
        ofield
          (\x tx -> tx {ctbrTreasuryDonation = fromSMaybe zero x})
          (D (decodePositiveCoin $ emptyFailure "Treasury Donation" "non-zero"))
      bodyFields n = invalidField n
      requiredFields :: [(Word, String)]
      requiredFields =
        [ (0, "inputs")
        , (1, "outputs")
        , (2, "fee")
        ]
      emptyFailure fieldName requirement =
        "TxBody: '" <> fieldName <> "' must be " <> requirement <> " when supplied"

newtype ConwayTxBody era = MkConwayTxBody (MemoBytes (ConwayTxBodyRaw era))
  deriving (Generic, SafeToHash, ToCBOR)

deriving newtype instance
  ( EraPParams era
  , EraTxCert era
  , DecCBOR (TxOut era)
  ) =>
  DecCBOR (ConwayTxBody era)

deriving instance
  (EraPParams era, NoThunks (TxOut era), NoThunks (TxCert era)) =>
  NoThunks (ConwayTxBody era)

deriving instance
  (EraPParams era, Eq (TxOut era), Eq (TxCert era)) =>
  Eq (ConwayTxBody era)

deriving newtype instance
  (EraPParams era, NFData (TxOut era), NFData (TxCert era)) =>
  NFData (ConwayTxBody era)

deriving instance
  (EraPParams era, Show (TxOut era), Show (TxCert era)) =>
  Show (ConwayTxBody era)

type instance MemoHashIndex (ConwayTxBodyRaw era) = EraIndependentTxBody

instance HashAnnotated (ConwayTxBody era) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

mkConwayTxBody :: forall era. ConwayEraTxBody era => ConwayTxBody era
mkConwayTxBody = mkMemoizedEra @era basicConwayTxBodyRaw

basicConwayTxBodyRaw :: ConwayTxBodyRaw era
basicConwayTxBodyRaw =
  ConwayTxBodyRaw
    mempty
    mempty
    mempty
    mempty
    SNothing
    SNothing
    OSet.empty
    (Withdrawals mempty)
    mempty
    (ValidityInterval SNothing SNothing)
    mempty
    mempty
    SNothing
    SNothing
    SNothing
    (VotingProcedures mempty)
    OSet.empty
    SNothing
    mempty

data ConwayTxBodyUpgradeError
  = CTBUETxCert ConwayTxCertUpgradeError
  | -- | The TxBody contains an update proposal from a pre-Conway era. Since
    --   this can only have come from the genesis delegates, we just discard it.
    CTBUEContainsUpdate
  | -- | In eras prior to Conway duplicate certificates where allowed
    CTBUEContainsDuplicateCerts (Set (TxCert ConwayEra))
  deriving (Eq, Show)

instance EraTxBody ConwayEra where
  type TxBody ConwayEra = ConwayTxBody ConwayEra
  type TxBodyUpgradeError ConwayEra = ConwayTxBodyUpgradeError

  mkBasicTxBody = mkConwayTxBody

  inputsTxBodyL = lensMemoRawType @ConwayEra ctbrSpendInputs $
    \txb x -> txb {ctbrSpendInputs = x}
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @ConwayEra (fmap sizedValue . ctbrOutputs) $
      \txb x -> txb {ctbrOutputs = mkSized (eraProtVerLow @ConwayEra) <$> x}
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensMemoRawType @ConwayEra ctbrFee (\txb x -> txb {ctbrFee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensMemoRawType @ConwayEra ctbrAuxDataHash $
    \txb x -> txb {ctbrAuxDataHash = x}
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = babbageSpendableInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = lensMemoRawType @ConwayEra ctbrWithdrawals $
    \txb x -> txb {ctbrWithdrawals = x}
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @ConwayEra (OSet.toStrictSeq . ctbrCerts) $
      \txb x -> txb {ctbrCerts = OSet.fromStrictSeq x}
  {-# INLINE certsTxBodyL #-}

  getTotalDepositsTxBody = conwayTotalDepositsTxBody

  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

  upgradeTxBody btb = do
    when (isSJust (btbUpdate btb)) $ Left CTBUEContainsUpdate
    certs <- traverse (left CTBUETxCert . upgradeTxCert) (btbCerts btb)
    let (duplicates, certsOSet) = OSet.fromStrictSeqDuplicates certs
    unless (null duplicates) $ Left $ CTBUEContainsDuplicateCerts duplicates
    pure $
      ConwayTxBody
        { ctbSpendInputs = btbInputs btb
        , ctbOutputs =
            mkSized (eraProtVerLow @ConwayEra)
              . upgradeTxOut
              . sizedValue
              <$> btbOutputs btb
        , ctbCerts = certsOSet
        , ctbWithdrawals = btbWithdrawals btb
        , ctbTxfee = btbTxFee btb
        , ctbVldt = btbValidityInterval btb
        , ctbAdHash = btbAuxDataHash btb
        , ctbMint = btbMint btb
        , ctbCollateralInputs = btbCollateral btb
        , ctbReqSignerHashes = btbReqSignerHashes btb
        , ctbScriptIntegrityHash = btbScriptIntegrityHash btb
        , ctbTxNetworkId = btbTxNetworkId btb
        , ctbReferenceInputs = btbReferenceInputs btb
        , ctbCollateralReturn =
            mkSized (eraProtVerLow @ConwayEra)
              . upgradeTxOut
              . sizedValue
              <$> btbCollateralReturn btb
        , ctbTotalCollateral = btbTotalCollateral btb
        , ctbCurrentTreasuryValue = SNothing
        , ctbProposalProcedures = OSet.empty
        , ctbVotingProcedures = VotingProcedures mempty
        , ctbTreasuryDonation = Coin 0
        }

-- ==========================================
-- Deposits and Refunds for Conway TxBody

-- | Compute all the deposits in a TxBody. This includes deposits for:
--
--   1. registering Stake
--   2. registering a StakePool
--   3. registering a DRep
--   4. submitting a Proposal
--
-- This is the contribution of a TxBody towards the total
-- `Cardano.Ledger.CertState.Obligations`
conwayTotalDepositsTxBody ::
  ConwayEraTxBody era =>
  PParams era ->
  (KeyHash 'StakePool -> Bool) ->
  TxBody era ->
  Coin
conwayTotalDepositsTxBody pp isPoolRegisted txBody =
  getTotalDepositsTxCerts pp isPoolRegisted (txBody ^. certsTxBodyL)
    <+> conwayProposalsDeposits pp txBody

-- | Total number of deposits in the proposals in TxBody
conwayProposalsDeposits ::
  ConwayEraTxBody era =>
  PParams era ->
  TxBody era ->
  Coin
conwayProposalsDeposits pp txBody = numProposals <×> depositPerProposal
  where
    numProposals = length (txBody ^. proposalProceduresTxBodyL)
    depositPerProposal = pp ^. ppGovActionDepositL

instance AllegraEraTxBody ConwayEra where
  vldtTxBodyL = lensMemoRawType @ConwayEra ctbrVldt $
    \txb x -> txb {ctbrVldt = x}
  {-# INLINE vldtTxBodyL #-}

instance MaryEraTxBody ConwayEra where
  mintTxBodyL = lensMemoRawType @ConwayEra ctbrMint $
    \txb x -> txb {ctbrMint = x}
  {-# INLINE mintTxBodyL #-}

  mintedTxBodyF = to $ \txBody -> policies (ctbrMint (getMemoRawType txBody))
  {-# INLINE mintedTxBodyF #-}

instance AlonzoEraTxBody ConwayEra where
  collateralInputsTxBodyL =
    lensMemoRawType @ConwayEra ctbrCollateralInputs $
      \txb x -> txb {ctbrCollateralInputs = x}
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensMemoRawType @ConwayEra ctbrReqSignerHashes $
      \txb x -> txb {ctbrReqSignerHashes = x}
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType @ConwayEra ctbrScriptIntegrityHash $
      \txb x -> txb {ctbrScriptIntegrityHash = x}
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = lensMemoRawType @ConwayEra ctbrNetworkId $
    \txb x -> txb {ctbrNetworkId = x}
  {-# INLINE networkIdTxBodyL #-}

  redeemerPointer = conwayRedeemerPointer

  redeemerPointerInverse = conwayRedeemerPointerInverse

instance BabbageEraTxBody ConwayEra where
  sizedOutputsTxBodyL = lensMemoRawType @ConwayEra ctbrOutputs $
    \txb x -> txb {ctbrOutputs = x}
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL =
    lensMemoRawType @ConwayEra ctbrReferenceInputs $
      \txb x -> txb {ctbrReferenceInputs = x}
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL =
    lensMemoRawType @ConwayEra ctbrTotalCollateral $
      \txb x -> txb {ctbrTotalCollateral = x}
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL =
    lensMemoRawType @ConwayEra (fmap sizedValue . ctbrCollateralReturn) $
      \txb x -> txb {ctbrCollateralReturn = mkSized (eraProtVerLow @ConwayEra) <$> x}
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL =
    lensMemoRawType @ConwayEra ctbrCollateralReturn $
      \txb x -> txb {ctbrCollateralReturn = x}
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = allSizedOutputsBabbageTxBodyF
  {-# INLINE allSizedOutputsTxBodyF #-}

instance ConwayEraTxBody ConwayEra where
  votingProceduresTxBodyL =
    lensMemoRawType @ConwayEra ctbrVotingProcedures $
      \txb x -> txb {ctbrVotingProcedures = x}
  {-# INLINE votingProceduresTxBodyL #-}
  proposalProceduresTxBodyL =
    lensMemoRawType @ConwayEra ctbrProposalProcedures $
      \txb x -> txb {ctbrProposalProcedures = x}
  {-# INLINE proposalProceduresTxBodyL #-}
  currentTreasuryValueTxBodyL =
    lensMemoRawType @ConwayEra ctbrCurrentTreasuryValue $
      \txb x -> txb {ctbrCurrentTreasuryValue = x}
  {-# INLINE currentTreasuryValueTxBodyL #-}
  treasuryDonationTxBodyL =
    lensMemoRawType @ConwayEra ctbrTreasuryDonation $
      \txb x -> txb {ctbrTreasuryDonation = x}
  {-# INLINE treasuryDonationTxBodyL #-}

instance
  (EraPParams era, Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (ConwayTxBody era)

pattern ConwayTxBody ::
  forall era.
  ConwayEraTxBody era =>
  Set TxIn ->
  Set TxIn ->
  Set TxIn ->
  StrictSeq (Sized (TxOut era)) ->
  StrictMaybe (Sized (TxOut era)) ->
  StrictMaybe Coin ->
  OSet.OSet (TxCert era) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  Set (KeyHash 'Witness) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  VotingProcedures era ->
  OSet.OSet (ProposalProcedure era) ->
  StrictMaybe Coin ->
  Coin ->
  ConwayTxBody era
pattern ConwayTxBody
  { ctbSpendInputs
  , ctbCollateralInputs
  , ctbReferenceInputs
  , ctbOutputs
  , ctbCollateralReturn
  , ctbTotalCollateral
  , ctbCerts
  , ctbWithdrawals
  , ctbTxfee
  , ctbVldt
  , ctbReqSignerHashes
  , ctbMint
  , ctbScriptIntegrityHash
  , ctbAdHash
  , ctbTxNetworkId
  , ctbVotingProcedures
  , ctbProposalProcedures
  , ctbCurrentTreasuryValue
  , ctbTreasuryDonation
  } <-
  ( getMemoRawType ->
      ConwayTxBodyRaw
        { ctbrSpendInputs = ctbSpendInputs
        , ctbrCollateralInputs = ctbCollateralInputs
        , ctbrReferenceInputs = ctbReferenceInputs
        , ctbrOutputs = ctbOutputs
        , ctbrCollateralReturn = ctbCollateralReturn
        , ctbrTotalCollateral = ctbTotalCollateral
        , ctbrCerts = ctbCerts
        , ctbrWithdrawals = ctbWithdrawals
        , ctbrFee = ctbTxfee
        , ctbrVldt = ctbVldt
        , ctbrReqSignerHashes = ctbReqSignerHashes
        , ctbrMint = ctbMint
        , ctbrScriptIntegrityHash = ctbScriptIntegrityHash
        , ctbrAuxDataHash = ctbAdHash
        , ctbrNetworkId = ctbTxNetworkId
        , ctbrVotingProcedures = ctbVotingProcedures
        , ctbrProposalProcedures = ctbProposalProcedures
        , ctbrCurrentTreasuryValue = ctbCurrentTreasuryValue
        , ctbrTreasuryDonation = ctbTreasuryDonation
        }
    )
  where
    ConwayTxBody
      inputsX
      collateralX
      referenceInputsX
      outputsX
      collateralReturnX
      totalCollateralX
      certsX
      withdrawalsX
      txfeeX
      vldtX
      reqSignerHashesX
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX
      votingProcedures
      proposalProcedures
      currentTreasuryValue
      treasuryDonation =
        mkMemoizedEra @era $
          ConwayTxBodyRaw
            inputsX
            collateralX
            referenceInputsX
            outputsX
            collateralReturnX
            totalCollateralX
            certsX
            withdrawalsX
            txfeeX
            vldtX
            reqSignerHashesX
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX
            votingProcedures
            proposalProcedures
            currentTreasuryValue
            treasuryDonation

{-# COMPLETE ConwayTxBody #-}

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

encodeTxBodyRaw ::
  ConwayEraTxBody era =>
  ConwayTxBodyRaw era ->
  Encode ('Closed 'Sparse) (ConwayTxBodyRaw era)
encodeTxBodyRaw ConwayTxBodyRaw {..} =
  let ValidityInterval bot top = ctbrVldt
   in Keyed
        ( \i ci ri o cr tc f t c w b ->
            ConwayTxBodyRaw i ci ri o cr tc c w f (ValidityInterval b t)
        )
        !> Key 0 (To ctbrSpendInputs)
        !> Omit null (Key 13 (To ctbrCollateralInputs))
        !> Omit null (Key 18 (To ctbrReferenceInputs))
        !> Key 1 (To ctbrOutputs)
        !> encodeKeyedStrictMaybe 16 ctbrCollateralReturn
        !> encodeKeyedStrictMaybe 17 ctbrTotalCollateral
        !> Key 2 (To ctbrFee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit OSet.null (Key 4 (To ctbrCerts))
        !> Omit (null . unWithdrawals) (Key 5 (To ctbrWithdrawals))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To ctbrReqSignerHashes))
        !> Omit (== mempty) (Key 9 (To ctbrMint))
        !> encodeKeyedStrictMaybe 11 ctbrScriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 ctbrAuxDataHash
        !> encodeKeyedStrictMaybe 15 ctbrNetworkId
        !> Omit (null . unVotingProcedures) (Key 19 (To ctbrVotingProcedures))
        !> Omit OSet.null (Key 20 (To ctbrProposalProcedures))
        !> encodeKeyedStrictMaybe 21 ctbrCurrentTreasuryValue
        !> Omit (== mempty) (Key 22 $ To ctbrTreasuryDonation)

instance ConwayEraTxBody era => EncCBOR (ConwayTxBodyRaw era) where
  encCBOR = encode . encodeTxBodyRaw

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (ConwayTxBody era)

class
  (BabbageEraTxBody era, ConwayEraTxCert era, ConwayEraPParams era, ConwayEraScript era) =>
  ConwayEraTxBody era
  where
  -- | Lens for getting and setting number of `Coin` that is expected to be in the
  -- Treasury at the current Epoch
  currentTreasuryValueTxBodyL :: Lens' (TxBody era) (StrictMaybe Coin)

  -- | Lens for getting and setting `VotingProcedures`.
  votingProceduresTxBodyL :: Lens' (TxBody era) (VotingProcedures era)

  -- | Lens for getting and setting `ProposalProcedures`.
  proposalProceduresTxBodyL :: Lens' (TxBody era) (OSet.OSet (ProposalProcedure era))

  treasuryDonationTxBodyL :: Lens' (TxBody era) Coin

conwayRedeemerPointer ::
  forall era.
  ConwayEraTxBody era =>
  TxBody era ->
  ConwayPlutusPurpose AsItem era ->
  StrictMaybe (ConwayPlutusPurpose AsIx era)
conwayRedeemerPointer txBody = \case
  ConwayMinting policyID ->
    ConwayMinting <$> indexOf policyID (txBody ^. mintedTxBodyF)
  ConwaySpending txIn ->
    ConwaySpending <$> indexOf txIn (txBody ^. inputsTxBodyL)
  ConwayRewarding rewardAccount ->
    ConwayRewarding <$> indexOf rewardAccount (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  ConwayCertifying txCert ->
    ConwayCertifying <$> indexOf txCert (txBody ^. certsTxBodyL)
  ConwayVoting votingProcedure ->
    ConwayVoting <$> indexOf votingProcedure (txBody ^. votingProceduresTxBodyL)
  ConwayProposing proposalProcedure ->
    ConwayProposing <$> indexOf proposalProcedure (txBody ^. proposalProceduresTxBodyL)

conwayRedeemerPointerInverse ::
  ConwayEraTxBody era =>
  TxBody era ->
  ConwayPlutusPurpose AsIx era ->
  StrictMaybe (ConwayPlutusPurpose AsIxItem era)
conwayRedeemerPointerInverse txBody = \case
  ConwayMinting idx ->
    ConwayMinting <$> fromIndex idx (txBody ^. mintedTxBodyF)
  ConwaySpending idx ->
    ConwaySpending <$> fromIndex idx (txBody ^. inputsTxBodyL)
  ConwayRewarding idx ->
    ConwayRewarding <$> fromIndex idx (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  ConwayCertifying idx ->
    ConwayCertifying <$> fromIndex idx (txBody ^. certsTxBodyL)
  ConwayVoting idx ->
    ConwayVoting <$> fromIndex idx (txBody ^. votingProceduresTxBodyL)
  ConwayProposing idx ->
    ConwayProposing <$> fromIndex idx (txBody ^. proposalProceduresTxBodyL)
