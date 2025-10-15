{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    dtbGuards
  ),
  upgradeProposals,
  upgradeGovAction,
  DijkstraTxBodyRaw (..),
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
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData (..))
import Data.Coerce (coerce)
import Data.OSet.Strict (OSet, decodeOSet)
import qualified Data.OSet.Strict as OSet
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set, foldr')
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to, (^.))
import NoThunks.Class (InspectHeap (..), NoThunks)

data DijkstraTxBodyRaw l era where
  DijkstraTxBodyRaw ::
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
    } ->
    DijkstraTxBodyRaw TopTx era
  DijkstraSubTxBodyRaw ::
    { dstbrSpendInputs :: !(Set TxIn)
    , dstbrReferenceInputs :: !(Set TxIn)
    , dstbrOutputs :: !(StrictSeq (Sized (TxOut DijkstraEra)))
    , dstbrCerts :: !(OSet.OSet (TxCert DijkstraEra))
    , dstbrWithdrawals :: !Withdrawals
    , dstbrVldt :: !ValidityInterval
    , dstbrGuards :: !(OSet (Credential Guard))
    , dstbrMint :: !MultiAsset
    , dstbrScriptIntegrityHash :: !(StrictMaybe ScriptIntegrityHash)
    , dstbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , dstbrNetworkId :: !(StrictMaybe Network)
    , dstbrVotingProcedures :: !(VotingProcedures DijkstraEra)
    , dstbrProposalProcedures :: !(OSet.OSet (ProposalProcedure DijkstraEra))
    , dstbrCurrentTreasuryValue :: !(StrictMaybe Coin)
    , dstbrTreasuryDonation :: !Coin
    } ->
    DijkstraTxBodyRaw SubTx era

deriving instance Eq (DijkstraTxBodyRaw l DijkstraEra)

instance EqRaw (TxBody l DijkstraEra)

deriving via
  InspectHeap (DijkstraTxBodyRaw l DijkstraEra)
  instance
    Typeable l => NoThunks (DijkstraTxBodyRaw l DijkstraEra)

instance NFData (DijkstraTxBodyRaw l DijkstraEra) where
  rnf = undefined

deriving instance Show (DijkstraTxBodyRaw l DijkstraEra)

basicDijkstraTxBodyRaw :: DijkstraTxBodyRaw TopTx DijkstraEra
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

instance Typeable l => DecCBOR (DijkstraTxBodyRaw l DijkstraEra) where
  decCBOR = withSTxBothLevels @l $ \case
    STopTx ->
      decode $
        SparseKeyed
          "TxBodyRaw"
          basicDijkstraTxBodyRaw
          bodyFields
          requiredFields
      where
        bodyFields :: Word -> Field (DijkstraTxBodyRaw l DijkstraEra)
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
    SSubTx -> undefined

encodeTxBodyRaw ::
  DijkstraTxBodyRaw l DijkstraEra ->
  Encode ('Closed 'Sparse) (DijkstraTxBodyRaw l DijkstraEra)
encodeTxBodyRaw DijkstraTxBodyRaw {..} =
  let ValidityInterval bot top = dtbrVldt
   in Keyed
        ( \i ci ri o cr tc f t c w b ->
            DijkstraTxBodyRaw i ci ri o cr tc c w f (ValidityInterval b t)
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
encodeTxBodyRaw DijkstraSubTxBodyRaw {..} =
  undefined

instance EncCBOR (DijkstraTxBodyRaw l DijkstraEra) where
  encCBOR = encode . encodeTxBodyRaw

deriving instance Typeable l => NoThunks (TxBody l DijkstraEra)

deriving instance Eq (TxBody l DijkstraEra)

deriving newtype instance NFData (TxBody l DijkstraEra)

deriving instance Show (TxBody l DijkstraEra)

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
  TxBody TopTx DijkstraEra
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
      treasuryDonation =
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

{-# COMPLETE DijkstraTxBody #-}

instance Memoized (TxBody l DijkstraEra) where
  type RawType (TxBody l DijkstraEra) = DijkstraTxBodyRaw l DijkstraEra

deriving newtype instance EncCBOR (TxBody l DijkstraEra)

type instance MemoHashIndex (DijkstraTxBodyRaw l DijkstraEra) = EraIndependentTxBody

instance HashAnnotated (TxBody l DijkstraEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

instance Typeable l => DecCBOR (Annotator (DijkstraTxBodyRaw l DijkstraEra)) where
  decCBOR = pure <$> decCBOR

deriving via
  Mem (DijkstraTxBodyRaw l DijkstraEra)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l DijkstraEra))

instance HasEraTxLevel TxBody DijkstraEra where
  toSTxLevel = undefined

instance EraTxBody DijkstraEra where
  newtype TxBody l DijkstraEra = MkDijkstraTxBody (MemoBytes (DijkstraTxBodyRaw l DijkstraEra))
    deriving (Generic, SafeToHash, ToCBOR)

  mkBasicTxBody =
    asSTxBothLevels
      (mkMemoizedEra @DijkstraEra basicDijkstraTxBodyRaw)
      undefined

  inputsTxBodyL = bothTxLensMemoRawType undefined undefined undefined undefined
  -- lensMemoRawType @DijkstraEra dtbrSpendInputs $
  --  \txb x -> txb {dtbrSpendInputs = x}
  {-# INLINE inputsTxBodyL #-}

  outputsTxBodyL =
    bothTxLensMemoRawType
      (fmap sizedValue . dtbrOutputs)
      (\x y -> x {dtbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> y})
      (fmap sizedValue . dstbrOutputs)
      (\x y -> x {dstbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> y})
  -- lensMemoRawType @DijkstraEra (fmap sizedValue . dtbrOutputs) $
  --   \txb x -> txb {dtbrOutputs = mkSized (eraProtVerLow @DijkstraEra) <$> x}
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
  PParams DijkstraEra -> (KeyHash StakePool -> Bool) -> TxBody l DijkstraEra -> Coin
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
  forall era l.
  DijkstraEraTxBody era =>
  TxBody l era ->
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
  TxBody l era ->
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

bothTxLensMemoRawType ::
  forall era a l.
  ( Era era
  , Typeable l
  , EncCBOR (RawType (TxBody TopTx era))
  , Memoized (TxBody TopTx era)
  , EncCBOR (RawType (TxBody SubTx era))
  , Memoized (TxBody SubTx era)
  ) =>
  (RawType (TxBody TopTx era) -> a) ->
  (RawType (TxBody TopTx era) -> a -> RawType (TxBody TopTx era)) ->
  (RawType (TxBody SubTx era) -> a) ->
  (RawType (TxBody SubTx era) -> a -> RawType (TxBody SubTx era)) ->
  Lens' (TxBody l era) a
bothTxLensMemoRawType getTop setTop getSub setSub = withSTxBothLevels @l $ \case
  STopTx -> lensMemoRawType @era getTop setTop
  SSubTx -> lensMemoRawType @era getSub setSub

instance AllegraEraTxBody DijkstraEra where
  vldtTxBodyL = bothTxLensMemoRawType dtbrVldt undefined dstbrVldt undefined
  -- lensMemoRawType @DijkstraEra dtbrVldt $
  --  \txb x -> txb {dtbrVldt = x}
  {-# INLINE vldtTxBodyL #-}

instance MaryEraTxBody DijkstraEra where
  mintTxBodyL =
    lensMemoRawType @DijkstraEra dtbrMint $
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

class ConwayEraTxBody era => DijkstraEraTxBody era where
  guardsTxBodyL :: Lens' (TxBody l era) (OSet (Credential Guard))

instance DijkstraEraTxBody DijkstraEra where
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
