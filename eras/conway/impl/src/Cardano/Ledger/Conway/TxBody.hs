{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Cardano.Ledger.Conway.TxCert (ConwayEraTxCert)
import Cardano.Ledger.Conway.TxOut (upgradeBabbageTxOut)
import Cardano.Ledger.Mary.Value (MultiAsset (..))
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
import Control.DeepSeq (NFData (..), deepseq)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OSet.Strict as OSet
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', to, (^.))
import NoThunks.Class (InspectHeap (..), NoThunks)

instance Memoized (TxBody l ConwayEra) where
  type RawType (TxBody l ConwayEra) = ConwayTxBodyRaw l ConwayEra

data ConwayTxBodyRaw l era where
  ConwayTxBodyRaw ::
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
    , ctbrReqSignerHashes :: !(Set (KeyHash 'Guard))
    , ctbrMint :: !MultiAsset
    , ctbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , ctbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , ctbrNetworkId :: !(StrictMaybe Network)
    , ctbrVotingProcedures :: !(VotingProcedures era)
    , ctbrProposalProcedures :: !(OSet.OSet (ProposalProcedure era))
    , ctbrCurrentTreasuryValue :: !(StrictMaybe Coin)
    , ctbrTreasuryDonation :: !Coin
    } ->
    ConwayTxBodyRaw TopTx era

deriving instance Eq (ConwayTxBodyRaw l ConwayEra)

deriving via
  InspectHeap (ConwayTxBodyRaw l ConwayEra)
  instance
    Typeable l => NoThunks (ConwayTxBodyRaw l ConwayEra)

instance NFData (ConwayTxBodyRaw l ConwayEra) where
  rnf ConwayTxBodyRaw {..} =
    ctbrSpendInputs `deepseq`
      ctbrCollateralInputs `deepseq`
        ctbrReferenceInputs `deepseq`
          ctbrOutputs `deepseq`
            ctbrCollateralReturn `deepseq`
              ctbrTotalCollateral `deepseq`
                ctbrCerts `deepseq`
                  ctbrWithdrawals `deepseq`
                    ctbrFee `deepseq`
                      ctbrVldt `deepseq`
                        ctbrReqSignerHashes `deepseq`
                          ctbrMint `deepseq`
                            ctbrScriptIntegrityHash `deepseq`
                              ctbrAuxDataHash `deepseq`
                                ctbrNetworkId `deepseq`
                                  ctbrVotingProcedures `deepseq`
                                    ctbrProposalProcedures `deepseq`
                                      ctbrCurrentTreasuryValue `deepseq`
                                        rnf ctbrTreasuryDonation

deriving instance Show (ConwayTxBodyRaw l ConwayEra)

instance HasEraTxLevel ConwayTxBodyRaw ConwayEra where
  toSTxLevel ConwayTxBodyRaw {} = STopTxOnly

instance Typeable l => DecCBOR (ConwayTxBodyRaw l ConwayEra) where
  decCBOR =
    fmap asSTxTopLevel . decode $
      SparseKeyed
        "TxBodyRaw"
        (asSTxTopLevel basicConwayTxBodyRaw)
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field (ConwayTxBodyRaw TopTx ConwayEra)
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

instance Typeable l => DecCBOR (Annotator (ConwayTxBodyRaw l ConwayEra)) where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (ConwayTxBodyRaw l ConwayEra)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l ConwayEra))

deriving instance Typeable l => NoThunks (TxBody l ConwayEra)

deriving instance Eq (TxBody l ConwayEra)

deriving newtype instance NFData (TxBody l ConwayEra)

deriving instance Show (TxBody l ConwayEra)

type instance MemoHashIndex (ConwayTxBodyRaw l ConwayEra) = EraIndependentTxBody

instance HashAnnotated (TxBody l ConwayEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

mkConwayTxBody :: Typeable l => TxBody l ConwayEra
mkConwayTxBody = mkMemoizedEra @ConwayEra $ asSTxTopLevel basicConwayTxBodyRaw

basicConwayTxBodyRaw :: ConwayTxBodyRaw TopTx ConwayEra
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

instance HasEraTxLevel TxBody ConwayEra where
  toSTxLevel = toSTxLevel . getMemoRawType

instance EraTxBody ConwayEra where
  newtype TxBody l ConwayEra = MkConwayTxBody (MemoBytes (ConwayTxBodyRaw l ConwayEra))
    deriving (Generic, SafeToHash, ToCBOR)

  mkBasicTxBody = mkConwayTxBody

  inputsTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrSpendInputs} -> ctbrSpendInputs) $
    \txb x -> txb {ctbrSpendInputs = x}
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @ConwayEra (fmap sizedValue . (\ConwayTxBodyRaw {ctbrOutputs} -> ctbrOutputs)) $
      \txb x -> txb {ctbrOutputs = mkSized (eraProtVerLow @ConwayEra) <$> x}
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensMemoRawType @ConwayEra ctbrFee (\txb x -> txb {ctbrFee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrAuxDataHash} -> ctbrAuxDataHash) $
    \txb x -> txb {ctbrAuxDataHash = x}
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = to (`withTopTxLevelOnly` (^. babbageSpendableInputsTxBodyF))
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrWithdrawals} -> ctbrWithdrawals) $
    \txb x -> txb {ctbrWithdrawals = x}
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @ConwayEra (OSet.toStrictSeq . (\ConwayTxBodyRaw {ctbrCerts} -> ctbrCerts)) $
      \txb x -> txb {ctbrCerts = OSet.fromStrictSeq x}
  {-# INLINE certsTxBodyL #-}

  getTotalDepositsTxBody = conwayTotalDepositsTxBody

  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

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
  TxBody l ConwayEra ->
  Coin
conwayTotalDepositsTxBody pp isPoolRegisted txBody =
  getTotalDepositsTxCerts pp isPoolRegisted (txBody ^. certsTxBodyL)
    <+> conwayProposalsDeposits pp txBody

-- | Total number of deposits in the proposals in TxBody
conwayProposalsDeposits ::
  ConwayEraTxBody era =>
  PParams era ->
  TxBody l era ->
  Coin
conwayProposalsDeposits pp txBody = numProposals <×> depositPerProposal
  where
    numProposals = length (txBody ^. proposalProceduresTxBodyL)
    depositPerProposal = pp ^. ppGovActionDepositL

instance AllegraEraTxBody ConwayEra where
  vldtTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrVldt} -> ctbrVldt) $
    \txb x -> txb {ctbrVldt = x}
  {-# INLINE vldtTxBodyL #-}

instance MaryEraTxBody ConwayEra where
  mintTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrMint} -> ctbrMint) $
    \txb x -> txb {ctbrMint = x}
  {-# INLINE mintTxBodyL #-}

instance AlonzoEraTxBody ConwayEra where
  collateralInputsTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrCollateralInputs} -> ctbrCollateralInputs) $
      \txb x -> txb {ctbrCollateralInputs = x}
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrReqSignerHashes} -> ctbrReqSignerHashes) $
      \txb x -> txb {ctbrReqSignerHashes = x}
  {-# INLINE reqSignerHashesTxBodyL #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrScriptIntegrityHash} -> ctbrScriptIntegrityHash) $
      \txb x -> txb {ctbrScriptIntegrityHash = x}
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrNetworkId} -> ctbrNetworkId) $
    \txb x -> txb {ctbrNetworkId = x}
  {-# INLINE networkIdTxBodyL #-}

  redeemerPointer = conwayRedeemerPointer

  redeemerPointerInverse = conwayRedeemerPointerInverse

instance BabbageEraTxBody ConwayEra where
  sizedOutputsTxBodyL = lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrOutputs} -> ctbrOutputs) $
    \txb x -> txb {ctbrOutputs = x}
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrReferenceInputs} -> ctbrReferenceInputs) $
      \txb x -> txb {ctbrReferenceInputs = x}
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrTotalCollateral} -> ctbrTotalCollateral) $
      \txb x -> txb {ctbrTotalCollateral = x}
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL =
    lensMemoRawType @ConwayEra
      (fmap sizedValue . (\ConwayTxBodyRaw {ctbrCollateralReturn} -> ctbrCollateralReturn))
      $ \txb x -> txb {ctbrCollateralReturn = mkSized (eraProtVerLow @ConwayEra) <$> x}
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrCollateralReturn} -> ctbrCollateralReturn) $
      \txb x -> txb {ctbrCollateralReturn = x}
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = to (`withTopTxLevelOnly` (^. allSizedOutputsBabbageTxBodyF))
  {-# INLINE allSizedOutputsTxBodyF #-}

instance ConwayEraTxBody ConwayEra where
  votingProceduresTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrVotingProcedures} -> ctbrVotingProcedures) $
      \txb x -> txb {ctbrVotingProcedures = x}
  {-# INLINE votingProceduresTxBodyL #-}
  proposalProceduresTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrProposalProcedures} -> ctbrProposalProcedures) $
      \txb x -> txb {ctbrProposalProcedures = x}
  {-# INLINE proposalProceduresTxBodyL #-}
  currentTreasuryValueTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrCurrentTreasuryValue} -> ctbrCurrentTreasuryValue) $
      \txb x -> txb {ctbrCurrentTreasuryValue = x}
  {-# INLINE currentTreasuryValueTxBodyL #-}
  treasuryDonationTxBodyL =
    lensMemoRawType @ConwayEra (\ConwayTxBodyRaw {ctbrTreasuryDonation} -> ctbrTreasuryDonation) $
      \txb x -> txb {ctbrTreasuryDonation = x}
  {-# INLINE treasuryDonationTxBodyL #-}

instance EqRaw (TxBody l ConwayEra)

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
  Set (KeyHash 'Guard) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  VotingProcedures ConwayEra ->
  OSet.OSet (ProposalProcedure ConwayEra) ->
  StrictMaybe Coin ->
  Coin ->
  TxBody TopTx ConwayEra
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
  ConwayTxBodyRaw l ConwayEra ->
  Encode ('Closed 'Sparse) (ConwayTxBodyRaw l ConwayEra)
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

instance EncCBOR (ConwayTxBodyRaw l ConwayEra) where
  encCBOR = encode . encodeTxBodyRaw

-- | Encodes memoized bytes created upon construction.
deriving newtype instance EncCBOR (TxBody l ConwayEra)

class
  (BabbageEraTxBody era, ConwayEraTxCert era, ConwayEraPParams era, ConwayEraScript era) =>
  ConwayEraTxBody era
  where
  -- | Lens for getting and setting number of `Coin` that is expected to be in the
  -- Treasury at the current Epoch
  currentTreasuryValueTxBodyL :: Lens' (TxBody l era) (StrictMaybe Coin)

  -- | Lens for getting and setting `VotingProcedures`.
  votingProceduresTxBodyL :: Lens' (TxBody l era) (VotingProcedures era)

  -- | Lens for getting and setting `ProposalProcedures`.
  proposalProceduresTxBodyL :: Lens' (TxBody l era) (OSet.OSet (ProposalProcedure era))

  treasuryDonationTxBodyL :: Lens' (TxBody l era) Coin

conwayRedeemerPointer ::
  forall era l.
  ConwayEraTxBody era =>
  TxBody l era ->
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
  TxBody l era ->
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
