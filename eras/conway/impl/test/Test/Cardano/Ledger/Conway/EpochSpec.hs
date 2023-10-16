{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conway.EpochSpec (spec) where

import Cardano.Ledger.BaseTypes (textToUrl)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (
  Constitution (..),
  DRepVotingThresholds (..),
  EraGov (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  PParams,
 )
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
  Committee (..),
  ConwayEraGov (..),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifyState (RatifyState),
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (VotingProcedures),
  cgEnactStateL,
  ensCommitteeL,
  epochStateDRepPulsingStateL,
  epochStateIncrStakeDistrL,
  gasDRepVotesL,
  getPulsingStateDRepDistr,
  rsDelayed,
  rsEnactState,
  rsRemoved,
  snapshotLookupId,
 )
import Cardano.Ledger.Conway.PParams (ppCommitteeMaxTermLengthL, ppDRepVotingThresholdsL)
import Cardano.Ledger.Conway.Rules (committeeAccepted, committeeAcceptedRatio, dRepAccepted, dRepAcceptedRatio, prevActionAsExpected, spoAccepted, validCommitteeTerm, withdrawalCanWithdraw)
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayEraTxCert (mkRegDRepTxCert),
  ConwayTxCert (..),
  Delegatee (DelegStakeVote),
  pattern AuthCommitteeHotKeyTxCert,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), coerceKeyRole)
import Cardano.Ledger.Shelley.LedgerState (
  IncrementalStake (..),
  certVStateL,
  curPParamsEpochStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  nesPdL,
  prevPParamsEpochStateL,
  totalObligation,
  utxosDepositedL,
  utxosGovStateL,
  utxosStakeDistrL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.TxIn (TxId)
import Cardano.Ledger.Val (Val (..))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Sequence.Strict as Seq
import qualified Data.Text as T
import Lens.Micro (to, (%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Binary.TreeDiff (showExpr)
import Test.Cardano.Ledger.Common (
  HasCallStack,
  Spec,
  describe,
  shouldBe,
  shouldSatisfy,
 )
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))

-- | Modify the PParams in the current state with the given function
modifyPParams :: EraGov era => (PParams era -> PParams era) -> ImpTestM era ()
modifyPParams f = do
  modifyNES $ nesEsL . curPParamsEpochStateL %~ f
  modifyNES $ nesEsL . prevPParamsEpochStateL %~ f

-- | Submit a transaction that registers a new DRep and return the keyhash
-- belonging to that DRep
registerDRep :: ImpTestM Conway (KeyHash 'DRepRole StandardCrypto)
registerDRep = do
  -- Register a DRep
  khDRep <- freshKeyHash
  _ <- submitBasicConwayTx "register DRep" $ \tx ->
    tx
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton
          ( mkRegDRepTxCert
              (KeyHashObj khDRep)
              zero
              SNothing
          )
  dreps <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  impIO $ dreps `shouldSatisfy` Map.member (KeyHashObj khDRep)
  pure khDRep

-- | Registers a new DRep and delegates 1 ADA to it. Returns the keyhash of the
-- DRep
setupSingleDRep :: ImpTestM Conway (KeyHash 'DRepRole StandardCrypto)
setupSingleDRep = do
  khDRep <- registerDRep

  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  kpSpending <- lookupKeyPair =<< freshKeyHash
  _ <- submitBasicConwayTx "Delegate to DRep" $ \tx ->
    tx
      & bodyTxL . outputsTxBodyL
        .~ Seq.singleton
          ( mkBasicTxOut
              (mkAddr (kpSpending, kpDelegator))
              (inject $ Coin 1000000)
          )
      & bodyTxL . certsTxBodyL
        .~ Seq.fromList
          [ ConwayTxCertDeleg $
              ConwayRegDelegCert
                (KeyHashObj khDelegator)
                (DelegStakeVote (coerceKeyRole khDRep) (DRepCredential $ KeyHashObj khDRep))
                zero
          ]
  pure khDRep

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter
voteForProposal ::
  Voter StandardCrypto ->
  GovActionId StandardCrypto ->
  ImpTestM Conway (TxId StandardCrypto)
voteForProposal voter gaId = do
  submitBasicConwayTx "Vote as DRep" $ \tx ->
    tx
      & bodyTxL . votingProceduresTxBodyL
        .~ VotingProcedures
          ( Map.singleton
              voter
              ( Map.singleton
                  gaId
                  ( VotingProcedure
                      { vProcVote = VoteYes
                      , vProcAnchor = SNothing
                      }
                  )
              )
          )

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe :: HasCallStack => String -> ImpTestM Conway ()
constitutionShouldBe cUrl = do
  constitution <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . to getConstitution
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    impIOMsg "Expecting a constitution" $ do
      pure $
        fromMaybe
          (error "No constitution has been set")
          constitution
  impIO $ anchorUrl `shouldBe` fromJust (textToUrl $ T.pack cUrl)

-- | Looks up the governance action state corresponding to the governance
-- action id
lookupGovActionState :: HasCallStack => GovActionId StandardCrypto -> ImpTestM Conway (GovActionState Conway)
lookupGovActionState aId = do
  proposals <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL
  impIOMsg "Expecting an action state" $ do
    maybe (error $ "Could not find action state for action " <> show aId) pure $
      snapshotLookupId aId proposals

-- | Builds a RatifyState from the current state
getRatifyEnv :: ImpTestM Conway (RatifyEnv Conway)
getRatifyEnv = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . getPulsingStateDRepDistr
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
calculateDRepAcceptedRatio :: GovActionId StandardCrypto -> ImpTestM Conway Rational
calculateDRepAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- lookupGovActionState gaId
  pure $
    dRepAcceptedRatio @Conway
      ratEnv
      (gas ^. gasDRepVotesL)
      (gasAction gas)

-- | Calculates the ratio of CC members that have voted for the governance
-- action
calculateCommitteeAcceptedRatio :: GovActionId StandardCrypto -> ImpTestM Conway Rational
calculateCommitteeAcceptedRatio gaId = do
  eNo <- getsNES nesELL
  RatifyEnv {reCommitteeState} <- getRatifyEnv
  GovActionState {gasCommitteeVotes} <- lookupGovActionState gaId
  committee <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
  let
    members = foldMap' committeeMembers committee
  pure $
    committeeAcceptedRatio
      members
      gasCommitteeVotes
      reCommitteeState
      eNo

-- | Logs the ratios of accepted votes per category
logAcceptedRatio :: GovActionId StandardCrypto -> ImpTestM Conway ()
logAcceptedRatio aId = do
  dRepRatio <- calculateDRepAcceptedRatio aId
  committeeRatio <- calculateCommitteeAcceptedRatio aId
  logEntry "----- ACCEPTED RATIOS -----"
  logEntry $ "DRep accepted ratio:\t\t" <> show dRepRatio
  logEntry $ "Committee accepted ratio:\t" <> show committeeRatio
  logEntry ""

-- | Checks whether the governance action has enough votes to be accepted
isGovActionAccepted :: GovActionId StandardCrypto -> ImpTestM Conway Bool
isGovActionAccepted gaId = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . getPulsingStateDRepDistr
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  action <- lookupGovActionState gaId
  enactSt <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  let
    ratEnv =
      RatifyEnv
        { reStakePoolDistr = poolDistr
        , reStakeDistr = credMap stakeDistr
        , reDRepState = drepState
        , reDRepDistr = drepDistr
        , reCurrentEpoch = eNo - 1
        , reCommitteeState = committeeState
        }
    ratSt =
      RatifyState
        { rsRemoved = mempty
        , rsEnactState = enactSt
        , rsDelayed = False
        }
  pure $ dRepAccepted ratEnv ratSt action

-- | Logs the current stake distribution
logStakeDistr :: ImpTestM era ()
logStakeDistr = do
  stakeDistr <- getsNES $ nesEsL . epochStateIncrStakeDistrL
  logEntry $ "Stake distr: " <> showExpr stakeDistr

-- | Logs the results of each check required to make the governance action pass
logRatificationChecks :: GovActionId StandardCrypto -> ImpTestM Conway ()
logRatificationChecks gaId = do
  gas@GovActionState {gasAction} <- lookupGovActionState gaId
  ens@EnactState {..} <-
    getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL
  ratEnv <- getRatifyEnv
  let
    ratSt = RatifyState ens mempty False
  currentEpoch <- getsNES nesELL
  logEntry "----- RATIFICATION CHECKS -----"
  logEntry $ "prevActionAsExpected:\t" <> show (prevActionAsExpected gasAction ensPrevGovActionIds)
  logEntry $ "validCommitteeTerm:\t" <> show (validCommitteeTerm ensCommittee ensPParams currentEpoch)
  logEntry "notDelayed:\t\t??"
  logEntry $ "withdrawalCanWithdraw:\t" <> show (withdrawalCanWithdraw gasAction ensTreasury)
  logEntry $ "committeeAccepted:\t" <> show (committeeAccepted ratSt ratEnv gas)
  logEntry $ "spoAccepted:\t\t" <> show (spoAccepted ratSt ratEnv gas)
  logEntry $ "dRepAccepted:\t\t" <> show (dRepAccepted ratEnv ratSt gas)
  logEntry ""

-- | Submits a transaction that registers a hot key for the given cold key.
-- Returns the hot key hash.
registerCCHotKey ::
  KeyHash 'ColdCommitteeRole StandardCrypto ->
  ImpTestM Conway (KeyHash 'HotCommitteeRole StandardCrypto)
registerCCHotKey coldKey = do
  hotKey <- freshKeyHash
  _ <- submitBasicConwayTx "Registering hot key" $ \tx ->
    tx
      & bodyTxL . certsTxBodyL
        .~ Seq.singleton (AuthCommitteeHotKeyTxCert (KeyHashObj coldKey) (KeyHashObj hotKey))
  pure hotKey

spec :: HasCallStack => Spec
spec =
  describe "Ratify traces" $ do
    itM @Conway "Runs basic transaction" $ do
      do
        certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
        govState <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
        impIO $ totalObligation certState govState `shouldBe` zero
      do
        deposited <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
        impIO $ deposited `shouldBe` zero
      _ <- submitBasicConwayTx "simple transaction" id
      passEpoch

    itM @Conway "Crosses the epoch boundary" $ do
      do
        epoch <- getsNES nesELL
        impIO $ epoch `shouldBe` 0
      passEpoch
      do
        epoch <- getsNES nesELL
        impIO $ epoch `shouldBe` 1

    itM @Conway "DRep registration should succeed" $ do
      logEntry "Stake distribution before DRep registration:"
      logStakeDistr
      _ <- registerDRep
      logEntry "Stake distribution after DRep registration:"
      logStakeDistr
      passEpoch

    itM @Conway "constitution is accepted after two epochs" $ do
      logEntry "Setting up PParams and DRep"
      modifyPParams $ \pp ->
        pp
          & ppDRepVotingThresholdsL .~ def {dvtUpdateToConstitution = 1 %! 2}
          & ppCommitteeMaxTermLengthL .~ 10
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
      gaidCommitteeProp <- submitProposal committeeAction
      _ <- voteForProposal (DRepVoter $ KeyHashObj khDRep) gaidCommitteeProp

      let
        assertNoCommittee = do
          committee <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
          impIOMsg "There should not be a committee" $ committee `shouldBe` SNothing
      assertNoCommittee
      passEpoch
      assertNoCommittee
      passEpoch
      do
        committee <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
        impIOMsg "There should be a committee" $ committee `shouldSatisfy` isSJust
      logEntry "Submitting new constitution"
      constitutionHash <- freshSafeHash
      let
        constitutionAction =
          NewConstitution
            SNothing
            ( Constitution
                ( Anchor
                    (fromJust $ textToUrl "constitution.0")
                    constitutionHash
                )
                SNothing
            )
      gaidConstitutionProp <-
        submitProposal constitutionAction
      logRatificationChecks gaidConstitutionProp
      do
        isAccepted <- isGovActionAccepted gaidConstitutionProp
        impIOMsg "Gov action should not be accepted" $ isAccepted `shouldBe` False
      khCommitteeMemberHot <- registerCCHotKey khCommitteeMember
      _ <- voteForProposal (DRepVoter $ KeyHashObj khDRep) gaidConstitutionProp
      _ <- voteForProposal (CommitteeVoter $ KeyHashObj khCommitteeMemberHot) gaidConstitutionProp
      logAcceptedRatio gaidConstitutionProp
      do
        isAccepted <- isGovActionAccepted gaidConstitutionProp
        impIOMsg "Gov action should be accepted" $ isAccepted `shouldBe` True

      passEpoch
      do
        isAccepted <- isGovActionAccepted gaidConstitutionProp
        impIOMsg "Gov action should be accepted" $ isAccepted `shouldBe` True
      logAcceptedRatio gaidConstitutionProp
      logRatificationChecks gaidConstitutionProp
      constitutionShouldBe ""

      passEpoch
      constitutionShouldBe "constitution.0"
