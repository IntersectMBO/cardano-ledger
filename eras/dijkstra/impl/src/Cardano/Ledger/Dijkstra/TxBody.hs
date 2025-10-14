{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Cardano.Ledger.Dijkstra.TxBody (
  DijkstraEraTxBody (..),
  TxBody (
    MkDijkstraTxBody,
    DijkstraTxBody,
    dtbSpendInputs,
    dtbCollateralInputs,
    dtbReferenceInputs,
    dtbOutputs,
    dtbCollateralReturn,
    dtbTotalCollateral,
    dtbCerts,
    dtbWithdrawals,
    dtbTxfee,
    dtbVldt,
    dtbMint,
    dtbScriptIntegrityHash,
    dtbAdHash,
    dtbTxNetworkId,
    dtbVotingProcedures,
    dtbProposalProcedures,
    dtbCurrentTreasuryValue,
    dtbTreasuryDonation,
    dtbGuards,
    dtbSubTransactions
  ),
  upgradeProposals,
  upgradeGovAction,
  DijkstraTxBodyRaw (..),
  DijkstraSubTxBodyRaw (..),
) where

import Cardano.Ledger.Alonzo.TxBody (Indexable (..))
import Cardano.Ledger.Babbage.TxBody (
  allSizedOutputsBabbageTxBodyF,
  babbageAllInputsTxBodyF,
  babbageSpendableInputsTxBodyF,
 )
import Cardano.Ledger.BaseTypes (Network, StrictMaybe (..), fromSMaybe)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  Sized (..),
  ToCBOR,
  TokenType (..),
  liftST,
  mkSized,
  peekTokenType,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Density (..),
  Encode (..),
  Field,
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
import Cardano.Ledger.Coin (Coin, decodePositiveCoin)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  ProposalProcedure (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.TxBody (
  conwayProposalsDeposits,
 )
import Cardano.Ledger.Core (EraPParams (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts (DijkstraPlutusPurpose (..))
import Cardano.Ledger.Dijkstra.TxOut ()
import Cardano.Ledger.Keys (HasKeyRole (..))
import Cardano.Ledger.Mary.Value (MultiAsset, policies)
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (..),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.TxIn (TxId, TxIn)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import Data.Kind (Type)
import qualified Data.OMap.Strict as OMap
import Data.OSet.Strict (OSet, decodeOSet)
import qualified Data.OSet.Strict as OSet
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, foldr')
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (NoThunks)

data DijkstraTxBodyRaw = DijkstraTxBodyRaw
  { dtbrSpendInputs :: !(Set TxIn)
  , dtbrCollateralInputs :: !(Set TxIn)
  , dtbrReferenceInputs :: !(Set TxIn)
  , dtbrOutputs :: !(StrictSeq (Sized (TxOut DijkstraEra)))
  , dtbrCollateralReturn :: !(StrictMaybe (Sized (TxOut DijkstraEra)))
  , dtbrTotalCollateral :: !(StrictMaybe Coin)
  , dtbrCerts :: !(OSet.OSet (TxCert DijkstraEra))
  , dtbrWithdrawals :: !Withdrawals
  , dtbrFee :: !Coin
  , dtbrVldt :: !ValidityInterval
  , dtbrGuards :: !(OSet (Credential Guard))
  , dtbrMint :: !MultiAsset
  , dtbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
  , dtbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , dtbrNetworkId :: !(StrictMaybe Network)
  , dtbrVotingProcedures :: !(VotingProcedures DijkstraEra)
  , dtbrProposalProcedures :: !(OSet.OSet (ProposalProcedure DijkstraEra))
  , dtbrCurrentTreasuryValue :: !(StrictMaybe Coin)
  , dtbrTreasuryDonation :: !Coin
  , dtbrSubTransactions :: !(OMap.OMap TxId (SubTxBody DijkstraEra))
  }
  deriving (Generic)

deriving instance Eq DijkstraTxBodyRaw

instance EqRaw (TxBody DijkstraEra)

instance NoThunks DijkstraTxBodyRaw

instance NFData DijkstraTxBodyRaw

deriving instance Show DijkstraTxBodyRaw

basicDijkstraTxBodyRaw :: DijkstraTxBodyRaw
basicDijkstraTxBodyRaw =
  DijkstraTxBodyRaw
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
    mempty

instance DecCBOR DijkstraTxBodyRaw where
  decCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        basicDijkstraTxBodyRaw
        bodyFields
        requiredFields
    where
      bodyFields :: Word -> Field DijkstraTxBodyRaw
      bodyFields 0 = field (\x tx -> tx {dtbrSpendInputs = x}) From
      bodyFields 1 = field (\x tx -> tx {dtbrOutputs = x}) From
      bodyFields 2 = field (\x tx -> tx {dtbrFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {dtbrVldt = (dtbrVldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        fieldGuarded
          (emptyFailure "Certificates" "non-empty")
          OSet.null
          (\x tx -> tx {dtbrCerts = x})
          From
      bodyFields 5 =
        fieldGuarded
          (emptyFailure "Withdrawals" "non-empty")
          (null . unWithdrawals)
          (\x tx -> tx {dtbrWithdrawals = x})
          From
      bodyFields 7 = ofield (\x tx -> tx {dtbrAuxDataHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {dtbrVldt = (dtbrVldt tx) {invalidBefore = x}})
          From
      bodyFields 9 =
        fieldGuarded
          (emptyFailure "Mint" "non-empty")
          (== mempty)
          (\x tx -> tx {dtbrMint = x})
          From
      bodyFields 11 = ofield (\x tx -> tx {dtbrScriptIntegrityHash = x}) From
      bodyFields 13 =
        fieldGuarded
          (emptyFailure "Collateral Inputs" "non-empty")
          null
          (\x tx -> tx {dtbrCollateralInputs = x})
          From
      bodyFields 14 =
        ofield
          (\x tx -> tx {dtbrGuards = fromSMaybe mempty x})
          (D decodeGuards)
      bodyFields 15 = ofield (\x tx -> tx {dtbrNetworkId = x}) From
      bodyFields 16 = ofield (\x tx -> tx {dtbrCollateralReturn = x}) From
      bodyFields 17 = ofield (\x tx -> tx {dtbrTotalCollateral = x}) From
      bodyFields 18 =
        fieldGuarded
          (emptyFailure "Reference Inputs" "non-empty")
          null
          (\x tx -> tx {dtbrReferenceInputs = x})
          From
      bodyFields 19 =
        fieldGuarded
          (emptyFailure "VotingProcedures" "non-empty")
          (null . unVotingProcedures)
          (\x tx -> tx {dtbrVotingProcedures = x})
          From
      bodyFields 20 =
        fieldGuarded
          (emptyFailure "ProposalProcedures" "non-empty")
          OSet.null
          (\x tx -> tx {dtbrProposalProcedures = x})
          From
      bodyFields 21 = ofield (\x tx -> tx {dtbrCurrentTreasuryValue = x}) From
      bodyFields 22 =
        ofield
          (\x tx -> tx {dtbrTreasuryDonation = fromSMaybe zero x})
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

encodeTxBodyRaw ::
  DijkstraTxBodyRaw ->
  Encode ('Closed 'Sparse) DijkstraTxBodyRaw
encodeTxBodyRaw DijkstraTxBodyRaw {..} =
  let ValidityInterval bot top = dtbrVldt
   in Keyed
        ( \i ci ri o cr tc f t c w b stxs ->
            DijkstraTxBodyRaw i ci ri o cr tc c w f (ValidityInterval b t) stxs
        )
        !> Key 0 (To dtbrSpendInputs)
        !> Omit null (Key 13 (To dtbrCollateralInputs))
        !> Omit null (Key 18 (To dtbrReferenceInputs))
        !> Key 1 (To dtbrOutputs)
        !> encodeKeyedStrictMaybe 16 dtbrCollateralReturn
        !> encodeKeyedStrictMaybe 17 dtbrTotalCollateral
        !> Key 2 (To dtbrFee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit OSet.null (Key 4 (To dtbrCerts))
        !> Omit (null . unWithdrawals) (Key 5 (To dtbrWithdrawals))
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit null (Key 14 (To dtbrGuards))
        !> Omit (== mempty) (Key 9 (To dtbrMint))
        !> encodeKeyedStrictMaybe 11 dtbrScriptIntegrityHash
        !> encodeKeyedStrictMaybe 7 dtbrAuxDataHash
        !> encodeKeyedStrictMaybe 15 dtbrNetworkId
        !> Omit (null . unVotingProcedures) (Key 19 (To dtbrVotingProcedures))
        !> Omit OSet.null (Key 20 (To dtbrProposalProcedures))
        !> encodeKeyedStrictMaybe 21 dtbrCurrentTreasuryValue
        !> Omit (== mempty) (Key 22 $ To dtbrTreasuryDonation)
        !> undefined

instance EncCBOR DijkstraTxBodyRaw where
  encCBOR = encode . encodeTxBodyRaw

deriving instance NoThunks (TxBody DijkstraEra)

deriving instance Eq (TxBody DijkstraEra)

deriving newtype instance NFData (TxBody DijkstraEra)

deriving instance Show (TxBody DijkstraEra)

pattern DijkstraTxBody ::
  Set TxIn ->
  Set TxIn ->
  Set TxIn ->
  StrictSeq (Sized (TxOut DijkstraEra)) ->
  StrictMaybe (Sized (TxOut DijkstraEra)) ->
  StrictMaybe Coin ->
  OSet.OSet (TxCert DijkstraEra) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  OSet (Credential Guard) ->
  MultiAsset ->
  StrictMaybe ScriptIntegrityHash ->
  StrictMaybe TxAuxDataHash ->
  StrictMaybe Network ->
  VotingProcedures DijkstraEra ->
  OSet.OSet (ProposalProcedure DijkstraEra) ->
  StrictMaybe Coin ->
  Coin ->
  OMap.OMap TxId (SubTxBody DijkstraEra) ->
  TxBody DijkstraEra
pattern DijkstraTxBody
  { dtbSpendInputs
  , dtbCollateralInputs
  , dtbReferenceInputs
  , dtbOutputs
  , dtbCollateralReturn
  , dtbTotalCollateral
  , dtbCerts
  , dtbWithdrawals
  , dtbTxfee
  , dtbVldt
  , dtbGuards
  , dtbMint
  , dtbScriptIntegrityHash
  , dtbAdHash
  , dtbTxNetworkId
  , dtbVotingProcedures
  , dtbProposalProcedures
  , dtbCurrentTreasuryValue
  , dtbTreasuryDonation
  , dtbSubTransactions
  } <-
  ( getMemoRawType ->
      DijkstraTxBodyRaw
        { dtbrSpendInputs = dtbSpendInputs
        , dtbrCollateralInputs = dtbCollateralInputs
        , dtbrReferenceInputs = dtbReferenceInputs
        , dtbrOutputs = dtbOutputs
        , dtbrCollateralReturn = dtbCollateralReturn
        , dtbrTotalCollateral = dtbTotalCollateral
        , dtbrCerts = dtbCerts
        , dtbrWithdrawals = dtbWithdrawals
        , dtbrFee = dtbTxfee
        , dtbrVldt = dtbVldt
        , dtbrGuards = dtbGuards
        , dtbrMint = dtbMint
        , dtbrScriptIntegrityHash = dtbScriptIntegrityHash
        , dtbrAuxDataHash = dtbAdHash
        , dtbrNetworkId = dtbTxNetworkId
        , dtbrVotingProcedures = dtbVotingProcedures
        , dtbrProposalProcedures = dtbProposalProcedures
        , dtbrCurrentTreasuryValue = dtbCurrentTreasuryValue
        , dtbrTreasuryDonation = dtbTreasuryDonation
        , dtbrSubTransactions = dtbSubTransactions
        }
    )
  where
    DijkstraTxBody
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
      guards
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX
      votingProcedures
      proposalProcedures
      currentTreasuryValue
      treasuryDonation
      subTransactions =
        mkMemoizedEra @DijkstraEra $
          DijkstraTxBodyRaw
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
            guards
            mintX
            scriptIntegrityHashX
            adHashX
            txnetworkidX
            votingProcedures
            proposalProcedures
            currentTreasuryValue
            treasuryDonation
            subTransactions

{-# COMPLETE DijkstraTxBody #-}

instance Memoized (TxBody DijkstraEra) where
  type RawType (TxBody DijkstraEra) = DijkstraTxBodyRaw

instance EncCBOR (TxBody DijkstraEra)

type instance MemoHashIndex DijkstraTxBodyRaw = EraIndependentTxBody

instance HashAnnotated (TxBody DijkstraEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

instance DecCBOR (Annotator DijkstraTxBodyRaw) where
  decCBOR = pure <$> decCBOR

deriving via Mem DijkstraTxBodyRaw instance DecCBOR (Annotator (TxBody DijkstraEra))

instance EraTxBodyCommon DijkstraEra TxBody where
  inputsTxBodyL = lensMemoRawType @DijkstraEra dtbrSpendInputs $
    \txb x -> txb {dtbrSpendInputs = x}
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @DijkstraEra (fmap sizedValue . dtbrOutputs) $
      \txb x -> txb {dtbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> x}
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensMemoRawType @DijkstraEra dtbrFee (\txb x -> txb {dtbrFee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensMemoRawType @DijkstraEra dtbrAuxDataHash $
    \txb x -> txb {dtbrAuxDataHash = x}
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = babbageSpendableInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = lensMemoRawType @DijkstraEra dtbrWithdrawals $
    \txb x -> txb {dtbrWithdrawals = x}
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @DijkstraEra (OSet.toStrictSeq . dtbrCerts) $
      \txb x -> txb {dtbrCerts = OSet.fromStrictSeq x}
  {-# INLINE certsTxBodyL #-}

  getTotalDepositsTxBody = dijkstraTotalDepositsTxBody

  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

instance EraTxBodyCommon DijkstraEra SubTxBody where
  inputsTxBodyL = lensMemoRawType @DijkstraEra dtbrSpendInputs $
    \txb x -> txb {dtbrSpendInputs = x}
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @DijkstraEra (fmap sizedValue . dtbrOutputs) $
      \txb x -> txb {dtbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> x}
  {-# INLINE outputsTxBodyL #-}

  feeTxBodyL = lensMemoRawType @DijkstraEra dtbrFee (\txb x -> txb {dtbrFee = x})
  {-# INLINE feeTxBodyL #-}

  auxDataHashTxBodyL = lensMemoRawType @DijkstraEra dtbrAuxDataHash $
    \txb x -> txb {dtbrAuxDataHash = x}
  {-# INLINE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = babbageSpendableInputsTxBodyF
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = babbageAllInputsTxBodyF
  {-# INLINE allInputsTxBodyF #-}

  withdrawalsTxBodyL = lensMemoRawType @DijkstraEra dtbrWithdrawals $
    \txb x -> txb {dtbrWithdrawals = x}
  {-# INLINE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @DijkstraEra (OSet.toStrictSeq . dtbrCerts) $
      \txb x -> txb {dtbrCerts = OSet.fromStrictSeq x}
  {-# INLINE certsTxBodyL #-}

  getTotalDepositsTxBody = dijkstraTotalDepositsTxBody

  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

instance EraTxBody DijkstraEra where
  newtype TxBody DijkstraEra = MkDijkstraTxBody (MemoBytes DijkstraTxBodyRaw)
    deriving (Generic, SafeToHash, ToCBOR)

  mkBasicTxBody = mkMemoizedEra @DijkstraEra basicDijkstraTxBodyRaw

upgradeGovAction ::
  forall era.
  (AlonzoEraPParams era, EraPParams (PreviousEra era)) =>
  GovAction (PreviousEra era) ->
  GovAction era
upgradeGovAction (ParameterChange x y z) =
  ParameterChange (coerce x) (upgradePParamsUpdate (emptyUpgradePParamsUpdate @era) y) z
upgradeGovAction (HardForkInitiation x y) = HardForkInitiation (coerce x) y
upgradeGovAction (TreasuryWithdrawals x y) = TreasuryWithdrawals x y
upgradeGovAction (NoConfidence x) = NoConfidence x
upgradeGovAction (UpdateCommittee x y z w) = UpdateCommittee x y z w
upgradeGovAction (NewConstitution x y) = NewConstitution x (coerce y)
upgradeGovAction InfoAction = InfoAction

upgradeProposals :: ProposalProcedure ConwayEra -> ProposalProcedure DijkstraEra
upgradeProposals ProposalProcedure {..} =
  ProposalProcedure
    { pProcDeposit = pProcDeposit
    , pProcReturnAddr = pProcReturnAddr
    , pProcGovAction = upgradeGovAction pProcGovAction
    , pProcAnchor = pProcAnchor
    }

dijkstraTotalDepositsTxBody ::
  PParams DijkstraEra -> (KeyHash StakePool -> Bool) -> TxBody DijkstraEra -> Coin
dijkstraTotalDepositsTxBody pp isPoolRegisted txBody =
  getTotalDepositsTxCerts pp isPoolRegisted (txBody ^. certsTxBodyL)
    <+> conwayProposalsDeposits pp txBody

-- | This newtype wrapper lets us index into the guards with a ScriptHash. It
-- will return the index of the credential when using `indexOf` and the `fromIndex`
-- method returns a `Nothing` if the credential at the index being looked up is
-- actually a keyhash.
newtype GuardsScriptHashView = GuardsScriptHashView (OSet (Credential Guard))

instance Indexable ScriptHash GuardsScriptHashView where
  indexOf (AsItem sh) (GuardsScriptHashView s) =
    coerce $ indexOf @(Credential Guard) (AsItem $ ScriptHashObj sh) s
  fromIndex (AsIx i) (GuardsScriptHashView s) = toScriptHash =<< fromIndex @(Credential Guard) (AsIx i) s
    where
      toScriptHash (AsIxItem idx (ScriptHashObj sh)) = SJust $ AsIxItem idx sh
      toScriptHash _ = SNothing

dijkstraRedeemerPointer ::
  forall era.
  DijkstraEraTxBody era =>
  TxBody era ->
  DijkstraPlutusPurpose AsItem era ->
  StrictMaybe (DijkstraPlutusPurpose AsIx era)
dijkstraRedeemerPointer txBody = \case
  DijkstraMinting policyID ->
    DijkstraMinting <$> indexOf policyID (txBody ^. mintedTxBodyF)
  DijkstraSpending txIn ->
    DijkstraSpending <$> indexOf txIn (txBody ^. inputsTxBodyL)
  DijkstraRewarding rewardAccount ->
    DijkstraRewarding <$> indexOf rewardAccount (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  DijkstraCertifying txCert ->
    DijkstraCertifying <$> indexOf txCert (txBody ^. certsTxBodyL)
  DijkstraVoting votingProcedure ->
    DijkstraVoting <$> indexOf votingProcedure (txBody ^. votingProceduresTxBodyL)
  DijkstraProposing proposalProcedure ->
    DijkstraProposing <$> indexOf proposalProcedure (txBody ^. proposalProceduresTxBodyL)
  DijkstraGuarding scriptHash ->
    DijkstraGuarding
      <$> indexOf scriptHash (GuardsScriptHashView $ txBody ^. guardsTxBodyL)

dijkstraRedeemerPointerInverse ::
  DijkstraEraTxBody era =>
  TxBody era ->
  DijkstraPlutusPurpose AsIx era ->
  StrictMaybe (DijkstraPlutusPurpose AsIxItem era)
dijkstraRedeemerPointerInverse txBody = \case
  DijkstraMinting idx ->
    DijkstraMinting <$> fromIndex idx (txBody ^. mintedTxBodyF)
  DijkstraSpending idx ->
    DijkstraSpending <$> fromIndex idx (txBody ^. inputsTxBodyL)
  DijkstraRewarding idx ->
    DijkstraRewarding <$> fromIndex idx (unWithdrawals (txBody ^. withdrawalsTxBodyL))
  DijkstraCertifying idx ->
    DijkstraCertifying <$> fromIndex idx (txBody ^. certsTxBodyL)
  DijkstraVoting idx ->
    DijkstraVoting <$> fromIndex idx (txBody ^. votingProceduresTxBodyL)
  DijkstraProposing idx ->
    DijkstraProposing <$> fromIndex idx (txBody ^. proposalProceduresTxBodyL)
  DijkstraGuarding idx ->
    DijkstraGuarding <$> fromIndex idx (GuardsScriptHashView $ txBody ^. guardsTxBodyL)

instance AllegraEraTxBody DijkstraEra where
  vldtTxBodyL = lensMemoRawType @DijkstraEra dtbrVldt $
    \txb x -> txb {dtbrVldt = x}
  {-# INLINE vldtTxBodyL #-}

instance MaryEraTxBody DijkstraEra where
  mintTxBodyL = lensMemoRawType @DijkstraEra dtbrMint $
    \txb x -> txb {dtbrMint = x}
  {-# INLINE mintTxBodyL #-}

  mintedTxBodyF = to $ \txBody -> policies (dtbrMint (getMemoRawType txBody))
  {-# INLINE mintedTxBodyF #-}

instance AlonzoEraTxBody DijkstraEra where
  collateralInputsTxBodyL =
    lensMemoRawType @DijkstraEra dtbrCollateralInputs $
      \txb x -> txb {dtbrCollateralInputs = x}
  {-# INLINE collateralInputsTxBodyL #-}

  reqSignerHashesTxBodyL = notSupportedInThisEraL
  {-# INLINE reqSignerHashesTxBodyL #-}

  reqSignerHashesTxBodyG = guardsTxBodyL . to (foldr' insertKeyHash mempty . OSet.toSet)
    where
      insertKeyHash (KeyHashObj kh) = Set.insert $ coerceKeyRole kh
      insertKeyHash (ScriptHashObj _) = id
  {-# INLINE reqSignerHashesTxBodyG #-}

  scriptIntegrityHashTxBodyL =
    lensMemoRawType @DijkstraEra dtbrScriptIntegrityHash $
      \txb x -> txb {dtbrScriptIntegrityHash = x}
  {-# INLINE scriptIntegrityHashTxBodyL #-}

  networkIdTxBodyL = lensMemoRawType @DijkstraEra dtbrNetworkId $
    \txb x -> txb {dtbrNetworkId = x}
  {-# INLINE networkIdTxBodyL #-}

  redeemerPointer = dijkstraRedeemerPointer

  redeemerPointerInverse = dijkstraRedeemerPointerInverse

instance BabbageEraTxBody DijkstraEra where
  sizedOutputsTxBodyL = lensMemoRawType @DijkstraEra dtbrOutputs $
    \txb x -> txb {dtbrOutputs = x}
  {-# INLINE sizedOutputsTxBodyL #-}

  referenceInputsTxBodyL =
    lensMemoRawType @DijkstraEra dtbrReferenceInputs $
      \txb x -> txb {dtbrReferenceInputs = x}
  {-# INLINE referenceInputsTxBodyL #-}

  totalCollateralTxBodyL =
    lensMemoRawType @DijkstraEra dtbrTotalCollateral $
      \txb x -> txb {dtbrTotalCollateral = x}
  {-# INLINE totalCollateralTxBodyL #-}

  collateralReturnTxBodyL =
    lensMemoRawType @DijkstraEra (fmap sizedValue . dtbrCollateralReturn) $
      \txb x -> txb {dtbrCollateralReturn = mkSized (eraProtVerLow @DijkstraEra) <$> x}
  {-# INLINE collateralReturnTxBodyL #-}

  sizedCollateralReturnTxBodyL =
    lensMemoRawType @DijkstraEra dtbrCollateralReturn $
      \txb x -> txb {dtbrCollateralReturn = x}
  {-# INLINE sizedCollateralReturnTxBodyL #-}

  allSizedOutputsTxBodyF = allSizedOutputsBabbageTxBodyF
  {-# INLINE allSizedOutputsTxBodyF #-}

instance ConwayEraTxBody DijkstraEra where
  votingProceduresTxBodyL =
    lensMemoRawType @DijkstraEra dtbrVotingProcedures $
      \txb x -> txb {dtbrVotingProcedures = x}
  {-# INLINE votingProceduresTxBodyL #-}
  proposalProceduresTxBodyL =
    lensMemoRawType @DijkstraEra dtbrProposalProcedures $
      \txb x -> txb {dtbrProposalProcedures = x}
  {-# INLINE proposalProceduresTxBodyL #-}
  currentTreasuryValueTxBodyL =
    lensMemoRawType @DijkstraEra dtbrCurrentTreasuryValue $
      \txb x -> txb {dtbrCurrentTreasuryValue = x}
  {-# INLINE currentTreasuryValueTxBodyL #-}
  treasuryDonationTxBodyL =
    lensMemoRawType @DijkstraEra dtbrTreasuryDonation $
      \txb x -> txb {dtbrTreasuryDonation = x}
  {-# INLINE treasuryDonationTxBodyL #-}

class
  ( ConwayEraTxBody era
  , EraTxBodyCommon era SubTxBody
  ) =>
  DijkstraEraTxBody era
  where
  data SubTxBody era :: Type

  guardsTxBodyL :: Lens' (TxBody era) (OSet (Credential Guard))

instance DijkstraEraTxBody DijkstraEra where
  newtype SubTxBody DijkstraEra = MkDijkstraSubTxBody DijkstraSubTxBodyRaw
    deriving (Generic, Eq, NoThunks, NFData, Show)

  {-# INLINE guardsTxBodyL #-}
  guardsTxBodyL =
    lensMemoRawType @DijkstraEra dtbrGuards $
      \txb x -> txb {dtbrGuards = x}

-- | Decoder for decoding guards in a backwards-compatible manner. It peeks at
-- the first element and if it's a credential, it decodes the rest of the
-- elements as credentials. If the first element is a plain keyhash, it will
-- decode rest of the elements as keyhashes.
decodeGuards :: Decoder s (OSet (Credential Guard))
decodeGuards = do
  elementsAreCredentials <- liftST $ newSTRef Nothing
  let
    decodeElement = do
      liftST (readSTRef elementsAreCredentials) >>= \case
        Nothing -> do
          tokenType <- peekTokenType
          liftST . writeSTRef elementsAreCredentials . Just $ case tokenType of
            TypeListLen -> True
            TypeListLen64 -> True
            TypeListLenIndef -> True
            _ -> False
          decodeElement
        Just True -> decCBOR
        Just False -> KeyHashObj <$> decCBOR
  decodeOSet decodeElement

data DijkstraSubTxBodyRaw = DijkstraSubTxBodyRaw
  { dstbrSpendInputs :: !(Set TxIn)
  , dstbrCollateralInputs :: !(Set TxIn)
  , dstbrReferenceInputs :: !(Set TxIn)
  , dstbrOutputs :: !(StrictSeq (Sized (TxOut DijkstraEra)))
  , dstbrCollateralReturn :: !(StrictMaybe (Sized (TxOut DijkstraEra)))
  , dstbrTotalCollateral :: !(StrictMaybe Coin)
  , dstbrCerts :: !(OSet.OSet (TxCert DijkstraEra))
  , dstbrWithdrawals :: !Withdrawals
  , dstbrFee :: !Coin
  , dstbrVldt :: !ValidityInterval
  , dstbrMint :: !MultiAsset
  , dstbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
  , dstbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , dstbrNetworkId :: !(StrictMaybe Network)
  , dstbrVotingProcedures :: !(VotingProcedures DijkstraEra)
  , dstbrProposalProcedures :: !(OSet.OSet (ProposalProcedure DijkstraEra))
  , dstbrCurrentTreasuryValue :: !(StrictMaybe Coin)
  , dstbrTreasuryDonation :: !Coin
  }
  deriving (Generic, Eq, Show)

instance NoThunks DijkstraSubTxBodyRaw

instance NFData DijkstraSubTxBodyRaw

instance OMap.HasOKey TxId (SubTxBody DijkstraEra) where
  okeyL = lens undefined undefined
