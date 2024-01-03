{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.ImpTest (
  module ImpTest,
  ConwayEraImp,
  submitGovAction,
  submitGovAction_,
  submitProposal,
  submitFailingProposal,
  trySubmitGovAction,
  trySubmitProposal,
  submitVote,
  submitVote_,
  submitYesVote_,
  submitFailingVote,
  trySubmitVote,
  registerDRep,
  setupSingleDRep,
  conwayModifyPParams,
  getEnactState,
  getGovActionState,
  lookupGovActionState,
  expectPresentGovActionId,
  expectMissingGovActionId,
  getRatifyEnv,
  calculateDRepAcceptedRatio,
  calculateCommitteeAcceptedRatio,
  logAcceptedRatio,
  canGovActionBeDRepAccepted,
  logRatificationChecks,
  resignCommitteeColdKey,
  registerCommitteeHotKey,
  logCurPParams,
  electCommittee,
  electBasicCommittee,
) where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo,
  Network (..),
  ShelleyBase,
  StrictMaybe (..),
  inject,
 )
import Cardano.Ledger.CertState (DRep (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayEraGov (..),
  ConwayGovState,
  DRepPulsingState (DRComplete),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionPurpose (..),
  GovActionState (..),
  PrevGovActionId (..),
  ProposalProcedure (..),
  RatifyEnv (..),
  RatifyState (..),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  cgDRepPulsingStateL,
  cgEnactStateL,
  ensCommitteeL,
  ensCurPParamsL,
  epochStateDRepPulsingStateL,
  finishDRepPulser,
  gasDRepVotesL,
  proposalsLookupId,
  psDRepDistrG,
  rsEnactStateL,
  setCompleteDRepPulsingState,
  utxosGovStateL,
  votingCommitteeThreshold,
  votingDRepThreshold,
  votingStakePoolThreshold,
 )
import Cardano.Ledger.Conway.Rules (
  EnactSignal,
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  prevActionAsExpected,
  spoAccepted,
  spoAcceptedRatio,
  validCommitteeTerm,
  withdrawalCanWithdraw,
 )
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert (..),
  Delegatee (..),
  pattern AuthCommitteeHotKeyTxCert,
  pattern RegDRepTxCert,
  pattern ResignCommitteeColdTxCert,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  IncrementalStake (..),
  asTreasuryL,
  certVStateL,
  curPParamsEpochStateL,
  epochStateGovStateL,
  esAccountStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  nesPdL,
  newEpochStateGovStateL,
  utxosStakeDistrL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (STS (..))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (isSJust)
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Alonzo.ImpTest as ImpTest
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

-- | Modify the PParams in the current state with the given function
conwayModifyPParams ::
  ( EraGov era
  , GovState era ~ ConwayGovState era
  ) =>
  (PParams era -> PParams era) ->
  ImpTestM era ()
conwayModifyPParams f = modifyNES $ \nes ->
  nes
    & nesEsL . curPParamsEpochStateL %~ f
    & newEpochStateGovStateL . cgDRepPulsingStateL %~ modifyDRepPulser
  where
    modifyDRepPulser pulser =
      case finishDRepPulser pulser of
        (snapshot, ratifyState) ->
          DRComplete snapshot (ratifyState & rsEnactStateL . ensCurPParamsL %~ f)

instance
  ( Crypto c
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (ConwayEra c)
  where
  emptyImpNES rootCoin =
    let nes =
          emptyAlonzoImpNES rootCoin
            & nesEsL . curPParamsEpochStateL . ppDRepActivityL .~ EpochInterval 100
            & nesEsL . curPParamsEpochStateL . ppGovActionLifetimeL .~ EpochInterval 30
        epochState = nes ^. nesEsL
        ratifyState = def & rsEnactStateL .~ (epochState ^. epochStateGovStateL . cgEnactStateL)
     in nes & nesEsL .~ setCompleteDRepPulsingState def ratifyState epochState

  modifyPParams = conwayModifyPParams

class
  ( ShelleyEraImp era
  , ConwayEraGov era
  , ConwayEraTxBody era
  , STS (EraRule "ENACT" era)
  , BaseM (EraRule "ENACT" era) ~ ShelleyBase
  , State (EraRule "ENACT" era) ~ EnactState era
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  , Environment (EraRule "ENACT" era) ~ ()
  , ToExpr (PParamsHKD Identity era)
  ) =>
  ConwayEraImp era

instance
  ( Crypto c
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ConwayEraImp (ConwayEra c)

-- | Submit a transaction that registers a new DRep and return the keyhash
-- belonging to that DRep
registerDRep ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  ImpTestM era (KeyHash 'DRepRole (EraCrypto era))
registerDRep = do
  -- Register a DRep
  khDRep <- freshKeyHash
  _ <-
    submitTx "register DRep" $
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL
          .~ SSeq.singleton
            ( RegDRepTxCert
                (KeyHashObj khDRep)
                zero
                SNothing
            )
  dreps <- getsNES @era $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  dreps `shouldSatisfy` Map.member (KeyHashObj khDRep)
  pure khDRep

-- | Registers a new DRep and delegates 1 ADA to it. Returns the keyhash of the
-- DRep
setupSingleDRep ::
  forall era.
  ( ConwayEraTxCert era
  , ShelleyEraImp era
  ) =>
  ImpTestM era (KeyHash 'DRepRole (EraCrypto era))
setupSingleDRep = do
  khDRep <- registerDRep

  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  kpSpending <- lookupKeyPair =<< freshKeyHash
  _ <-
    submitTx "Delegate to DRep" $
      mkBasicTx mkBasicTxBody
        & bodyTxL . outputsTxBodyL
          .~ SSeq.singleton
            ( mkBasicTxOut
                (mkAddr (kpSpending, kpDelegator))
                (inject $ Coin 1000000)
            )
        & bodyTxL . certsTxBodyL
          .~ SSeq.fromList
            [ mkRegDepositDelegTxCert @era
                (KeyHashObj khDelegator)
                (DelegVote (DRepCredential $ KeyHashObj khDRep))
                zero
            ]
  pure khDRep

-- | Submits a transaction with a Vote for the given governance action as
-- some voter
submitVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Vote ->
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era (TxId (EraCrypto era))
submitVote vote voter gaId = trySubmitVote vote voter gaId >>= expectRightDeep

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter
submitYesVote_ ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
submitYesVote_ voter gaId = void $ submitVote VoteYes voter gaId

submitVote_ ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Vote ->
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
submitVote_ vote voter gaId = void $ submitVote vote voter gaId

submitFailingVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  [PredicateFailure (EraRule "LEDGER" era)] ->
  ImpTestM era ()
submitFailingVote voter gaId expectedFailure =
  trySubmitVote VoteYes voter gaId >>= (`shouldBeLeftExpr` expectedFailure)

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter, and expects an `Either` result.
trySubmitVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  Vote ->
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (TxId (EraCrypto era))
    )
trySubmitVote vote voter gaId = do
  trySubmitTx $
    mkBasicTx mkBasicTxBody
      & bodyTxL . votingProceduresTxBodyL
        .~ VotingProcedures
          ( Map.singleton
              voter
              ( Map.singleton
                  gaId
                  ( VotingProcedure
                      { vProcVote = vote
                      , vProcAnchor = SNothing
                      }
                  )
              )
          )

submitProposal ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  ProposalProcedure era ->
  ImpTestM era (GovActionId (EraCrypto era))
submitProposal proposal = trySubmitProposal proposal >>= expectRightExpr

-- | Submits a transaction that proposes the given proposal
trySubmitProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  ProposalProcedure era ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (GovActionId (EraCrypto era))
    )
trySubmitProposal proposal = do
  eitherTxId <-
    trySubmitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL . proposalProceduresTxBodyL .~ OSet.singleton proposal
  pure $ case eitherTxId of
    Right txId ->
      Right
        GovActionId
          { gaidTxId = txId
          , gaidGovActionIx = GovActionIx 0
          }
    Left err -> Left err

submitFailingProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  ProposalProcedure era ->
  [PredicateFailure (EraRule "LEDGER" era)] ->
  ImpTestM era ()
submitFailingProposal proposal expectedFailure =
  trySubmitProposal proposal >>= (`shouldBeLeftExpr` expectedFailure)

-- | Submits a transaction that proposes the given governance action
trySubmitGovAction ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  GovAction era ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (GovActionId (EraCrypto era))
    )
trySubmitGovAction ga = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  khPropRwd <- freshKeyHash
  trySubmitProposal $
    ProposalProcedure
      { pProcDeposit = pp ^. ppGovActionDepositL
      , pProcReturnAddr = RewardAcnt Testnet (KeyHashObj khPropRwd)
      , pProcGovAction = ga
      , pProcAnchor = def
      }

submitGovAction ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  GovAction era ->
  ImpTestM era (GovActionId (EraCrypto era))
submitGovAction ga = trySubmitGovAction ga >>= expectRightExpr

submitGovAction_ ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  GovAction era ->
  ImpTestM era ()
submitGovAction_ = void . submitGovAction

getEnactState :: ConwayEraGov era => ImpTestM era (EnactState era)
getEnactState = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . enactStateGovStateL

-- | Looks up the governance action state corresponding to the governance action id
lookupGovActionState ::
  ConwayEraGov era =>
  GovActionId (EraCrypto era) ->
  ImpTestM era (Maybe (GovActionState era))
lookupGovActionState aId = do
  proposals <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL
  pure $ proposalsLookupId aId proposals

-- | Looks up the governance action state corresponding to the governance action id
getGovActionState ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era (GovActionState era)
getGovActionState govActionId =
  impAnn "Expecting an action state" $ do
    lookupGovActionState govActionId >>= \case
      Nothing ->
        assertFailure $ "Could not find action state for govActionId: " <> show govActionId
      Just govActionState -> pure govActionState

expectPresentGovActionId ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
expectPresentGovActionId govActionId = void $ getGovActionState govActionId

expectMissingGovActionId ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
expectMissingGovActionId govActionId =
  impAnn "Expecting for gov action state to be missing" $ do
    lookupGovActionState govActionId >>= \case
      Just _ ->
        expectationFailure $ "Found gov action state for govActionId: " <> show govActionId
      Nothing -> pure ()

-- | Builds a RatifyState from the current state
getRatifyEnv :: ConwayEraGov era => ImpTestM era (RatifyEnv era)
getRatifyEnv = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  pure
    RatifyEnv
      { reStakePoolDistr = poolDistr
      , reStakeDistr = credMap stakeDistr
      , reDRepState = drepState
      , reDRepDistr = drepDistr
      , reCurrentEpoch = eNo - 1
      , reCommitteeState = committeeState
      }

-- | Calculates the ratio of DReps that have voted for the governance action
calculateDRepAcceptedRatio ::
  forall era.
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Rational
calculateDRepAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pure $
    dRepAcceptedRatio @era
      ratEnv
      (gas ^. gasDRepVotesL)
      (gasAction gas)

-- | Calculates the ratio of Committee members that have voted for the governance
-- action
calculateCommitteeAcceptedRatio ::
  forall era.
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Rational
calculateCommitteeAcceptedRatio gaId = do
  eNo <- getsNES nesELL
  RatifyEnv {reCommitteeState} <- getRatifyEnv
  GovActionState {gasCommitteeVotes} <- getGovActionState gaId
  ens <- getEnactState
  let
    committee = ens ^. ensCommitteeL
    members = foldMap' (committeeMembers @era) committee
  pure $
    committeeAcceptedRatio
      members
      gasCommitteeVotes
      reCommitteeState
      eNo

-- | Logs the ratios of accepted votes per category
logAcceptedRatio :: (HasCallStack, ConwayEraGov era) => GovActionId (EraCrypto era) -> ImpTestM era ()
logAcceptedRatio aId = do
  dRepRatio <- calculateDRepAcceptedRatio aId
  committeeRatio <- calculateCommitteeAcceptedRatio aId
  logEntry "----- ACCEPTED RATIOS -----"
  logEntry $ "DRep accepted ratio:\t\t" <> show dRepRatio
  logEntry $ "Committee accepted ratio:\t" <> show committeeRatio
  logEntry ""

-- | Checks whether the governance action has enough DRep votes to be accepted in the next
-- epoch. (Note that no other checks execept DRep votes is used)
canGovActionBeDRepAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Bool
canGovActionBeDRepAccepted gaId = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  action <- getGovActionState gaId
  enactSt <- getEnactState
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  let
    ratEnv =
      RatifyEnv
        { reStakePoolDistr = poolDistr
        , reStakeDistr = credMap stakeDistr
        , reDRepState = drepState
        , reDRepDistr = drepDistr
        , reCurrentEpoch = eNo
        , reCommitteeState = committeeState
        }
    ratSt =
      RatifyState
        { rsRemoved = mempty
        , rsEnacted = mempty
        , rsEnactState = enactSt
        , rsDelayed = False
        }
  pure $ dRepAccepted ratEnv ratSt action

-- | Logs the results of each check required to make the governance action pass
logRatificationChecks ::
  (ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
logRatificationChecks gaId = do
  gas@GovActionState {gasCommitteeVotes, gasDRepVotes, gasAction} <- getGovActionState gaId
  ens@EnactState {..} <- getEnactState
  ratEnv <- getRatifyEnv
  let
    ratSt = RatifyState ens mempty mempty False
  curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  currentEpoch <- getsNES nesELL
  let
    members = foldMap' committeeMembers (ens ^. ensCommitteeL)
    committeeState = reCommitteeState ratEnv
  logEntry $
    unlines
      [ "----- RATIFICATION CHECKS -----"
      , "prevActionAsExpected:\t" <> show (prevActionAsExpected gasAction ensPrevGovActionIds)
      , "validCommitteeTerm:\t" <> show (validCommitteeTerm gasAction ensCurPParams currentEpoch)
      , "notDelayed:\t\t??"
      , "withdrawalCanWithdraw:\t" <> show (withdrawalCanWithdraw gasAction curTreasury)
      , "committeeAccepted:\t\t"
          <> show (committeeAccepted ratEnv ratSt gas)
          <> " [To Pass: "
          <> show
            (committeeAcceptedRatio members gasCommitteeVotes committeeState currentEpoch)
          <> show " >= "
          <> show (votingCommitteeThreshold ratSt gasAction)
          <> show "]"
      , "spoAccepted:\t\t"
          <> show (spoAccepted ratEnv ratSt gas)
          <> " [To Pass: "
          <> show (spoAcceptedRatio ratEnv gas)
          <> " >= "
          <> show (votingStakePoolThreshold ratSt gasAction)
          <> "]"
      , "dRepAccepted:\t\t"
          <> show (dRepAccepted ratEnv ratSt gas)
          <> " [To Pass: "
          <> show (dRepAcceptedRatio ratEnv gasDRepVotes gasAction)
          <> " >= "
          <> show (votingDRepThreshold ratSt gasAction)
          <> "]"
      , ""
      ]

-- | Submits a transaction that registers a hot key for the given cold key.
-- Returns the hot key hash.
registerCommitteeHotKey ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  KeyHash 'ColdCommitteeRole (EraCrypto era) ->
  ImpTestM era (KeyHash 'HotCommitteeRole (EraCrypto era))
registerCommitteeHotKey coldKey = do
  hotKey <- freshKeyHash
  _ <-
    submitTx "Registering hot key" $
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL
          .~ SSeq.singleton (AuthCommitteeHotKeyTxCert (KeyHashObj coldKey) (KeyHashObj hotKey))
  pure hotKey

-- | Submits a transaction that resigns the cold key
resignCommitteeColdKey ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  KeyHash 'ColdCommitteeRole (EraCrypto era) ->
  ImpTestM era ()
resignCommitteeColdKey coldKey = do
  void $
    submitTx "Resigning cold key" $
      mkBasicTx mkBasicTxBody
        & bodyTxL . certsTxBodyL
          .~ SSeq.singleton (ResignCommitteeColdTxCert (KeyHashObj coldKey) SNothing)

electCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)) ->
  KeyHash 'DRepRole (EraCrypto era) ->
  Set.Set (KeyHash 'ColdCommitteeRole (EraCrypto era)) ->
  Map.Map (KeyHash 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
  ImpTestM era (PrevGovActionId 'CommitteePurpose (EraCrypto era))
electCommittee prevGovId drep toRemove toAdd = do
  let
    committeeAction =
      UpdateCommittee
        prevGovId
        (Set.map KeyHashObj toRemove)
        (Map.mapKeys KeyHashObj toAdd)
        (1 %! 2)
  gaidCommitteeProp <- submitGovAction committeeAction
  submitYesVote_ (DRepVoter $ KeyHashObj drep) gaidCommitteeProp
  pure (PrevGovActionId gaidCommitteeProp)

electBasicCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  ImpTestM era (Credential 'DRepRole (EraCrypto era), Credential 'HotCommitteeRole (EraCrypto era))
electBasicCommittee = do
  logEntry "Setting up PParams and DRep"
  modifyPParams $ \pp ->
    pp
      & ppDRepVotingThresholdsL
        .~ def
          { dvtCommitteeNormal = 1 %! 1
          , dvtCommitteeNoConfidence = 1 %! 2
          , dvtUpdateToConstitution = 1 %! 2
          }
      & ppCommitteeMaxTermLengthL .~ EpochInterval 10
      & ppGovActionLifetimeL .~ EpochInterval 2
      & ppGovActionDepositL .~ Coin 123
  khDRep <- setupSingleDRep

  logEntry "Registering committee member"
  khCommitteeMember <- freshKeyHash
  let
    committeeAction =
      UpdateCommittee
        SNothing
        mempty
        (Map.singleton (KeyHashObj khCommitteeMember) 10)
        (1 %! 2)
  gaidCommitteeProp <- submitGovAction committeeAction

  submitYesVote_ (DRepVoter $ KeyHashObj khDRep) gaidCommitteeProp

  let
    assertNoCommittee = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
      impAnn "There should not be a committee" $ committee `shouldBe` SNothing
  logRatificationChecks gaidCommitteeProp
  assertNoCommittee

  passEpoch
  logRatificationChecks gaidCommitteeProp
  assertNoCommittee
  passEpoch
  do
    committee <-
      getsNES $
        nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
    impAnn "There should be a committee" $ committee `shouldSatisfy` isSJust

  khCommitteeMemberHot <- registerCommitteeHotKey khCommitteeMember
  pure (KeyHashObj khDRep, KeyHashObj khCommitteeMemberHot)

logCurPParams :: (EraGov era, ToExpr (PParamsHKD Identity era)) => ImpTestM era ()
logCurPParams = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  logEntry $ "Current PParams:\n--------------" <> showExpr pp <> "\n--------------"
