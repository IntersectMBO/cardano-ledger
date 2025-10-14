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

module Cardano.Ledger.Conway.TxBody (
  ConwayEraTxBody (..),
  TxBody (
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
  conwayRedeemerPointer,
  conwayRedeemerPointerInverse,
  upgradeBabbageTxOut,
) where

import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.TxBody (
  allSizedOutputsBabbageTxBodyF,
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
 )
import Cardano.Ledger.BaseTypes (Network, fromSMaybe)
import Cardano.Ledger.Binary (
  Annotator,
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
 )
import Cardano.Ledger.Conway.TxOut (upgradeBabbageTxOut)
import Cardano.Ledger.Mary.Value (MultiAsset (..), policies)
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
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
import Control.DeepSeq (NFData)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro (Lens', to, (^.))
import NoThunks.Class (NoThunks)

instance Memoized (TxBody ConwayEra) where
  type RawType (TxBody ConwayEra) = ConwayTxBodyRaw

data ConwayTxBodyRaw = ConwayTxBodyRaw
  { ctbrSpendInputs :: !(Set TxIn)
  , ctbrCollateralInputs :: !(Set TxIn)
  , ctbrReferenceInputs :: !(Set TxIn)
  , ctbrOutputs :: !(StrictSeq (Sized (TxOut ConwayEra)))
  , ctbrCollateralReturn :: !(StrictMaybe (Sized (TxOut ConwayEra)))
  , ctbrTotalCollateral :: !(StrictMaybe Coin)
  , ctbrCerts :: !(OSet.OSet (TxCert ConwayEra))
  , ctbrWithdrawals :: !Withdrawals
  , ctbrFee :: !Coin
  , ctbrVldt :: !ValidityInterval
  , ctbrReqSignerHashes :: !(Set (KeyHash 'Witness))
  , ctbrMint :: !MultiAsset
  , ctbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
  , ctbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , ctbrNetworkId :: !(StrictMaybe Network)
  , ctbrVotingProcedures :: !(VotingProcedures ConwayEra)
  , ctbrProposalProcedures :: !(OSet.OSet (ProposalProcedure ConwayEra))
  , ctbrCurrentTreasuryValue :: !(StrictMaybe Coin)
  , ctbrTreasuryDonation :: !Coin
  }
  deriving (Generic)

deriving instance Eq ConwayTxBodyRaw

instance NoThunks ConwayTxBodyRaw

instance NFData ConwayTxBodyRaw

deriving instance Show ConwayTxBodyRaw

instance DecCBOR ConwayTxBodyRaw where
  decCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        basicConwayTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field ConwayTxBodyRaw
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

instance DecCBOR (Annotator ConwayTxBodyRaw) where
  decCBOR = pure <$> decCBOR

deriving via Mem ConwayTxBodyRaw instance DecCBOR (Annotator (TxBody ConwayEra))

deriving instance NoThunks (TxBody ConwayEra)

deriving instance Eq (TxBody ConwayEra)

deriving newtype instance NFData (TxBody ConwayEra)

deriving instance Show (TxBody ConwayEra)

type instance MemoHashIndex ConwayTxBodyRaw = EraIndependentTxBody

instance HashAnnotated (TxBody ConwayEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

mkConwayTxBody :: TxBody ConwayEra
mkConwayTxBody = mkMemoizedEra @ConwayEra basicConwayTxBodyRaw

basicConwayTxBodyRaw :: ConwayTxBodyRaw
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

instance EraTxBodyCommon ConwayEra TxBody where
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

instance EraTxBody ConwayEra where
  newtype TxBody ConwayEra = MkConwayTxBody (MemoBytes ConwayTxBodyRaw)
    deriving (Generic, SafeToHash, ToCBOR)

  mkBasicTxBody = mkConwayTxBody

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
  PParams ConwayEra ->
  (KeyHash 'StakePool -> Bool) ->
  TxBody ConwayEra ->
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

instance EqRaw (TxBody ConwayEra)

pattern ConwayTxBody ::
  Set TxIn ->
  Set TxIn ->
  Set TxIn ->
  StrictSeq (Sized (TxOut ConwayEra)) ->
  StrictMaybe (Sized (TxOut ConwayEra)) ->
  StrictMaybe Coin ->
  OSet.OSet (TxCert ConwayEra) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  Set (KeyHash 'Witness) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  VotingProcedures ConwayEra ->
  OSet.OSet (ProposalProcedure ConwayEra) ->
  StrictMaybe Coin ->
  Coin ->
  TxBody ConwayEra
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
        mkMemoizedEra @ConwayEra $
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
  ConwayTxBodyRaw ->
  Encode ('Closed 'Sparse) ConwayTxBodyRaw
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

instance EncCBOR ConwayTxBodyRaw where
  encCBOR = encode . encodeTxBodyRaw

-- | Encodes memoized bytes created upon construction.
instance EncCBOR (TxBody ConwayEra)

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
