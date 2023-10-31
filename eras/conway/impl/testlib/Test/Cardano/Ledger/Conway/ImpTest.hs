{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  submitProposal,
  submitFailingProposal,
  voteForProposal,
  registerDRep,
  setupSingleDRep,
  conwayModifyPParams,
  getEnactState,
  lookupGovActionState,
  getRatifyEnv,
  calculateDRepAcceptedRatio,
  calculateCommitteeAcceptedRatio,
  logAcceptedRatio,
  canGovActionBeDRepAccepted,
  logRatificationChecks,
  registerCommitteeHotKey,
  logCurPParams,
  tryVoteForProposal,
) where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes (Network (..), ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.CertState (DRep (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  Era (..),
  EraIndependentTxBody,
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  PParams,
 )
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayEraGov (..),
  ConwayGovState,
  DRepPulsingState (DRComplete),
  EnactState (..),
  GovAction,
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  ProposalProcedure (..),
  RatifyEnv (..),
  RatifyState (..),
  Vote (..),
  Voter,
  VotingProcedure (..),
  VotingProcedures (..),
  cgDRepPulsingStateL,
  cgEnactStateL,
  ensCommitteeL,
  ensCurPParamsL,
  epochStateDRepPulsingStateL,
  finishDRepPulser,
  gasDRepVotesL,
  psDRepDistrG,
  rsEnactStateL,
  setCompleteDRepPulsingState,
  snapshotLookupId,
  utxosGovStateL,
  votingDRepThreshold,
  votingStakePoolThreshold,
 )
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppDRepActivityL, ppGovActionLifetimeL)
import Cardano.Ledger.Conway.Rules (
  EnactSignal,
  committeeAccepted,
  committeeAcceptedRatio,
  conwayWitsVKeyNeeded,
  dRepAccepted,
  dRepAcceptedRatio,
  prevActionAsExpected,
  spoAccepted,
  spoAcceptedRatio,
  validCommitteeTerm,
  withdrawalCanWithdraw,
 )
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert (..),
  Delegatee (..),
  pattern AuthCommitteeHotKeyTxCert,
  pattern RegDRepTxCert,
 )
import Cardano.Ledger.Core (EraRule)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Governance (EraGov (GovState))
import Cardano.Ledger.Shelley.LedgerState (
  IncrementalStake (..),
  NewEpochState,
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
  utxosUtxoL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.TxIn (TxId)
import Cardano.Ledger.Val (Val (..))
import Control.State.Transition.Extended (STS (..))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import Lens.Micro ((%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Alonzo.ImpTest as ImpTest
import Test.Cardano.Ledger.Binary.TreeDiff (showExpr)
import Test.Cardano.Ledger.Common (HasCallStack, shouldSatisfy)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)

conwayImpWitsVKeyNeeded ::
  ( EraTx era
  , ConwayEraTxBody era
  ) =>
  NewEpochState era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
conwayImpWitsVKeyNeeded nes = conwayWitsVKeyNeeded utxo
  where
    utxo = nes ^. nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL

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
            & nesEsL . curPParamsEpochStateL . ppDRepActivityL .~ 100
            & nesEsL . curPParamsEpochStateL . ppGovActionLifetimeL .~ 30
        epochState = nes ^. nesEsL
        ratifyState = def & rsEnactStateL .~ (epochState ^. epochStateGovStateL . cgEnactStateL)
     in nes & nesEsL .~ setCompleteDRepPulsingState def ratifyState epochState

  impWitsVKeyNeeded = conwayImpWitsVKeyNeeded

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
  impIO $ dreps `shouldSatisfy` Map.member (KeyHashObj khDRep)
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

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter
voteForProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era (TxId (EraCrypto era))
voteForProposal voter gaId = do
  submitTx "Vote as DRep" $
    mkBasicTx mkBasicTxBody
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

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter, and expects an `Either` result.
tryVoteForProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
        (TxId (EraCrypto era))
    )
tryVoteForProposal voter gaId = do
  trySubmitTx $
    mkBasicTx mkBasicTxBody
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

-- | Submits a transaction that proposes the given governance action
trySubmitProposal ::
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
trySubmitProposal ga = do
  khPropRwd <- freshKeyHash
  eitherTxId <-
    trySubmitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL . proposalProceduresTxBodyL
          .~ OSet.singleton
            ProposalProcedure
              { pProcDeposit = zero
              , pProcReturnAddr =
                  RewardAcnt
                    Testnet
                    (KeyHashObj khPropRwd)
              , pProcGovAction = ga
              , pProcAnchor = def
              }
  pure $ case eitherTxId of
    Right txId ->
      Right
        GovActionId
          { gaidTxId = txId
          , gaidGovActionIx = GovActionIx 0
          }
    Left err -> Left err

submitProposal ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  GovAction era ->
  ImpTestM era (GovActionId (EraCrypto era))
submitProposal ga = trySubmitProposal ga >>= impExpectSuccess

submitFailingProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  GovAction era ->
  ImpTestM era ()
submitFailingProposal ga = trySubmitProposal ga >>= impExpectFailure

getEnactState :: ConwayEraGov era => ImpTestM era (EnactState era)
getEnactState = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . enactStateGovStateL

-- | Looks up the governance action state corresponding to the governance
-- action id
lookupGovActionState :: (HasCallStack, ConwayEraGov era) => GovActionId (EraCrypto era) -> ImpTestM era (GovActionState era)
lookupGovActionState aId = do
  proposals <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL
  impIOMsg "Expecting an action state" $ do
    maybe (error $ "Could not find action state for action " <> show aId) pure $
      snapshotLookupId aId proposals

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
  gas <- lookupGovActionState gaId
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
  GovActionState {gasCommitteeVotes} <- lookupGovActionState gaId
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
  action <- lookupGovActionState gaId
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
  gas@GovActionState {gasDRepVotes, gasAction} <- lookupGovActionState gaId
  ens@EnactState {..} <- getEnactState
  ratEnv <- getRatifyEnv
  let
    ratSt = RatifyState ens mempty False
  curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  currentEpoch <- getsNES nesELL
  logEntry $
    unlines
      [ "----- RATIFICATION CHECKS -----"
      , "prevActionAsExpected:\t" <> show (prevActionAsExpected gasAction ensPrevGovActionIds)
      , "validCommitteeTerm:\t" <> show (validCommitteeTerm ensCommittee ensCurPParams currentEpoch)
      , "notDelayed:\t\t??"
      , "withdrawalCanWithdraw:\t" <> show (withdrawalCanWithdraw gasAction curTreasury)
      , "committeeAccepted:\t" <> show (committeeAccepted ratEnv ratSt gas)
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

logCurPParams :: EraGov era => ImpTestM era ()
logCurPParams = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  logEntry $ "Current PParams:\n--------------" <> showExpr pp <> "\n--------------"
