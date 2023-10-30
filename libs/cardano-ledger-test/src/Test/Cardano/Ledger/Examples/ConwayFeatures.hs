{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.ConwayFeatures (conwayFeatures)
where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Ledger.Address (Addr (..), RewardAcnt (..))
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Network (..),
  StrictMaybe (..),
  mkTxIx,
  textToUrl,
 )
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.CertState (CommitteeState (..), vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  dvtUpdateToConstitutionL,
 )
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.DRep (DRep (..), DRepState (..), drepExpiryL)
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  coerceKeyRole,
  hashKey,
 )
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.Shelley.API (
  Hash,
  PoolDistr (..),
  VerKeyVRF,
  hashVerKeyVRF,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMap (unify)
import Cardano.Ledger.Val (Val (..), inject)
import Control.Exception (evaluate)
import Control.State.Transition.Extended hiding (Assertion)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as SSeq
import GHC.Stack
import Lens.Micro
import Test.Cardano.Ledger.Binary.TreeDiff (assertExprEqualWithMessage)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..))
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)
import qualified Test.Cardano.Ledger.Core.Utils as SLE
import Test.Cardano.Ledger.Examples.BabbageFeatures (
  InitOutputs (..),
  KeyPairRole (..),
  TestCaseData (..),
  txFromTestCaseData,
  utxoFromTestCaseData,
 )
import Test.Cardano.Ledger.Examples.STSTestUtils (
  mkGenesisTxIn,
  runEPOCH,
  runLEDGER,
  trustMeP,
 )
import Test.Cardano.Ledger.Generic.Fields (
  TxBodyField (..),
  TxOutField (..),
 )
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (Scriptic (..))
import Test.Cardano.Ledger.Generic.Updaters
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE
import Test.Cardano.Ledger.Shelley.Utils (
  RawSeed (..),
  mkKeyPair,
  mkKeyPair',
  mkVRFKeyPair,
 )
import Test.Cardano.Protocol.Crypto.VRF (VRFKeyPair (..))
import Test.Tasty
import Test.Tasty.HUnit

-- =================================================================

-- | Accesses the same data as psProposalsL, but converts from (StrictSeq (GovActionState era)) to (ProposalsSnapshot era)
psProposalsSnapshotL :: Lens' (PulsingSnapshot era) (ProposalsSnapshot era)
psProposalsSnapshotL =
  lens
    (fromGovActionStateSeq . psProposals)
    (\x y -> x {psProposals = snapshotActions y})

stakeKeyHash :: forall era. Era era => Proof era -> KeyHash 'Staking (EraCrypto era)
stakeKeyHash _pf = hashKey . snd $ mkKeyPair (RawSeed 0 0 0 0 2)

stakePoolKeys :: forall era. Era era => Proof era -> KeyPair 'StakePool (EraCrypto era)
stakePoolKeys _pf = mkKeyPair' @(EraCrypto era) (RawSeed 0 0 0 0 20)

drepKeys :: forall era. Era era => Proof era -> KeyPair 'DRepRole (EraCrypto era)
drepKeys _pf = mkKeyPair' @(EraCrypto era) (RawSeed 0 0 0 0 30)

drepCredential :: forall era. Era era => Proof era -> Credential 'DRepRole (EraCrypto era)
drepCredential pf = KeyHashObj . hashKey . vKey $ drepKeys pf

stakePoolKeyHash :: forall era. Era era => Proof era -> KeyHash 'StakePool (EraCrypto era)
stakePoolKeyHash pf = hashKey . vKey $ stakePoolKeys pf

ccHotKeys :: forall era. Era era => Proof era -> KeyPair 'HotCommitteeRole (EraCrypto era)
ccHotKeys _pf = mkKeyPair' @(EraCrypto era) (RawSeed 0 0 0 0 40)

ccHotCred :: forall era. Era era => Proof era -> Credential 'HotCommitteeRole (EraCrypto era)
ccHotCred pf = KeyHashObj . hashKey . vKey $ ccHotKeys pf

ccColdKeys :: forall era. Era era => Proof era -> KeyPair 'ColdCommitteeRole (EraCrypto era)
ccColdKeys _pf = mkKeyPair' @(EraCrypto era) (RawSeed 0 0 0 0 50)

ccColdCred :: forall era. Era era => Proof era -> Credential 'ColdCommitteeRole (EraCrypto era)
ccColdCred pf = KeyHashObj . hashKey . vKey $ ccColdKeys pf

keys1 :: forall era. Era era => Proof era -> KeyPair 'Payment (EraCrypto era)
keys1 _pf = mkKeyPair' @(EraCrypto era) (RawSeed 1 3 1 1 1)

addrKeys1 :: forall era. Era era => Proof era -> Addr (EraCrypto era)
addrKeys1 pf = Addr Testnet pCred sCred
  where
    pCred = KeyHashObj . hashKey . vKey $ keys1 pf
    sCred = StakeRefBase . KeyHashObj . coerceKeyRole . hashKey . vKey $ keys1 pf

keys2 :: forall era. Era era => Proof era -> KeyPair 'Payment (EraCrypto era)
keys2 _pf = mkKeyPair' @(EraCrypto era) (RawSeed 2 2 2 2 2)

-- | If this looks magic, it is. The purpose of this is to use it to add to the UMap
--   a mapping between a staking credential (in one of the TxOut's in the UTxO used)
--   and one of the DReps. If no one delegates to a DRep, No DRep will have any way
--   for their vote to count. Not how this is used to initialize the LedgerState
delegs :: Era era => Proof era -> Map.Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era))
delegs pf = Map.fromList [(KeyHashObj . coerceKeyRole . hashKey . vKey $ keys2 pf, DRepCredential (drepCredential pf))]

addrKeys2 :: forall era. Era era => Proof era -> Addr (EraCrypto era)
addrKeys2 pf = Addr Testnet pCred sCred
  where
    pCred = KeyHashObj . hashKey . vKey $ keys2 pf
    sCred = StakeRefBase . KeyHashObj . coerceKeyRole . hashKey . vKey $ keys2 pf

vrfKeyHash :: forall c. Crypto c => Hash c (VerKeyVRF c)
vrfKeyHash = hashVerKeyVRF . vrfVerKey . mkVRFKeyPair @c $ RawSeed 0 0 0 0 0

someTxIn :: (CH.HashAlgorithm (HASH c), HasCallStack) => TxIn c
someTxIn = mkGenesisTxIn 1

proposedConstitution :: forall era. Scriptic era => Constitution era
proposedConstitution = Constitution (SLE.mkDummyAnchor 1) SNothing

newConstitutionProposal :: forall era. Scriptic era => Proof era -> ProposalProcedure era
newConstitutionProposal pf =
  ProposalProcedure
    proposalDeposit
    (RewardAcnt Testnet (KeyHashObj (stakeKeyHash pf)))
    (NewConstitution SNothing (proposedConstitution @era))
    (Anchor (fromJust $ textToUrl "new.constitution.com") (SLE.mkDummySafeHash Proxy 1))

anotherConstitutionProposal ::
  forall era.
  Scriptic era =>
  Proof era ->
  GovActionId (EraCrypto era) ->
  ProposalProcedure era
anotherConstitutionProposal pf prevGovActionId =
  ProposalProcedure
    proposalDeposit
    (RewardAcnt Testnet (KeyHashObj (stakeKeyHash pf)))
    ( NewConstitution
        (SJust (PrevGovActionId prevGovActionId))
        (Constitution (SLE.mkDummyAnchor 2) SNothing)
    )
    (Anchor (fromJust $ textToUrl "another.constitution.com") (SLE.mkDummySafeHash Proxy 2))

voteYes :: forall era. Scriptic era => Proof era -> GovActionId (EraCrypto era) -> VotingProcedures era
voteYes pf govActionId =
  VotingProcedures $
    Map.fromList
      [ (DRepVoter (drepCredential pf), Map.fromList [(govActionId, VotingProcedure VoteYes SNothing)])
      , (CommitteeVoter (ccHotCred pf), Map.fromList [(govActionId, VotingProcedure VoteYes SNothing)])
      ]

govActionState ::
  GovActionId (EraCrypto era) ->
  ProposalProcedure era ->
  GovActionState era
govActionState gaid ProposalProcedure {..} =
  GovActionState
    gaid
    mempty
    mempty
    mempty
    pProcDeposit
    pProcReturnAddr
    pProcGovAction
    (EpochNo 0)
    (EpochNo 30)

expiringGovActionState ::
  EpochNo ->
  GovActionId (EraCrypto era) ->
  ProposalProcedure era ->
  GovActionState era
expiringGovActionState expiry govActionId pposal = govActionState govActionId pposal & gasExpiresAfterL .~ expiry

govActionStateWithYesVotes :: Scriptic era => GovActionId (EraCrypto era) -> Proof era -> ProposalProcedure era -> GovActionState era
govActionStateWithYesVotes gaid pf ProposalProcedure {..} =
  GovActionState
    gaid
    (Map.fromList [(ccHotCred pf, VoteYes)])
    (Map.fromList [(drepCredential pf, VoteYes)])
    mempty
    pProcDeposit
    pProcReturnAddr
    pProcGovAction
    (EpochNo 0)
    (EpochNo 30)

-- | Value for the actual threshold, plus a small epsilon for GT (>) relation
spoThreshold :: Rational
spoThreshold = 51 % 100 + 1 % 100000000000

pp :: ConwayEraPParams era => PParams era
pp =
  emptyPParams
    & ppMaxValSizeL .~ 1000000000
    & ppDRepActivityL .~ 100
    & ppGovActionLifetimeL .~ 30
    & ppGovActionDepositL .~ proposalDeposit
    & ppDRepVotingThresholdsL . dvtUpdateToConstitutionL
      .~ unsafeBoundRational (1 % 2)
    & ppCommitteeMaxTermLengthL .~ 100

fee :: Integer
fee = 5

proposalDeposit :: Coin
proposalDeposit = Coin 10

-- | Evaluates an `Either` value and fails immediately if the result is `Left`
expectRight :: (HasCallStack, Show a) => String -> Either a b -> IO b
expectRight msg = either (\x -> error (msg <> show x)) evaluate

proposal :: forall era. (Scriptic era, EraTxBody era) => Proof era -> TestCaseData era
proposal pf =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [someTxIn]
          , Collateral' []
          , Outputs'
              [ newTxOut pf [Address (addrKeys2 pf), Amount (inject $ Coin (15000 - 10000 - fee) <-> proposalDeposit)] -- 4985
              , newTxOut pf [Address (addrKeys1 pf), Amount (inject $ Coin 10000)]
              ]
          , Txfee (Coin fee)
          , GovProcs (GovProcedures (VotingProcedures mempty) [newConstitutionProposal pf])
          ]
    , initOutputs =
        InitOutputs
          { ofInputs =
              [ newTxOut
                  pf
                  [ Address (addrKeys1 pf)
                  , Amount (inject $ Coin 15000)
                  ]
              ]
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [KeyPairPayment (keys1 pf)]
    , otherWitsFields = []
    }

secondProposal :: forall era. (Scriptic era, EraTxBody era) => Proof era -> GovActionId (EraCrypto era) -> TestCaseData era
secondProposal pf govActionId =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [TxIn (gaidTxId govActionId) (mkTxIx 1)]
          , Collateral' []
          , Outputs'
              [ newTxOut pf [Address (addrKeys2 pf), Amount (inject $ Coin (10000 - 7000 - fee) <-> proposalDeposit)]
              , newTxOut pf [Address (addrKeys1 pf), Amount (inject $ Coin 7000)]
              ]
          , Txfee (Coin fee)
          , GovProcs
              ( GovProcedures
                  (VotingProcedures mempty)
                  [anotherConstitutionProposal pf govActionId]
              )
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = []
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits = [KeyPairPayment (keys1 pf)]
    , otherWitsFields = []
    }

vote ::
  forall era.
  ( Scriptic era
  , EraTxBody era
  , TxCert era ~ ConwayTxCert era
  ) =>
  Proof era ->
  GovActionId (EraCrypto era) ->
  TestCaseData era
vote pf govActionId =
  TestCaseData
    { txBody =
        newTxBody
          pf
          [ Inputs' [TxIn (gaidTxId govActionId) (mkTxIx 0)]
          , Outputs'
              [ newTxOut pf [Address (addrKeys1 pf), Amount (inject $ Coin (4985 - 1995 - fee))]
              , newTxOut pf [Address (addrKeys2 pf), Amount (inject $ Coin 1995)]
              ]
          , Txfee (Coin fee)
          , GovProcs (GovProcedures (voteYes pf govActionId) [])
          , Certs' [ConwayTxCertGov (ConwayRegDRep (drepCredential pf) (Coin 0) SNothing)]
          ]
    , initOutputs =
        InitOutputs
          { ofInputs = []
          , ofRefInputs = []
          , ofCollateral = []
          }
    , keysForAddrWits =
        [ KeyPairPayment (keys2 pf)
        , KeyPairStakePool (stakePoolKeys pf)
        , KeyPairDRep (drepKeys pf)
        , KeyPairCommittee (ccHotKeys pf)
        ]
    , otherWitsFields = []
    }

-- =====================================================
-- Test that an epoch without proposals does not cause the the
-- time that it takes for a DRep to expire become smaller
-- =====================================================

-- | Test that the expiration of Proposals works. We start with 1 proposal in the current
-- state, and 0 proposals in the previous state (inside the DRepPulser) The one current
-- proposal is proposed in Epoch 0, and expires after Epoch 2. So the number of proposals
-- in each Epoch should look like this:
--
--    Epoch   current  prev
--    0       1        0
--    1       1        1
--    2       1        1
--    3       1        1
--    4       0        0
preventDRepExpiry ::
  forall era.
  ( Reflect era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "EPOCH" era))
  , Scriptic era
  , ConwayEraTxBody era
  , GovState era ~ ConwayGovState era
  , ConwayEraGov era
  ) =>
  Proof era ->
  Assertion
preventDRepExpiry pf = do
  let
    (utxo0, _) = utxoFromTestCaseData pf (proposal pf)
    pp' = pp & ppGovActionLifetimeL .~ 3
    proposalTx = txFromTestCaseData pf (proposal pf)
    govActionId = GovActionId (txid (proposalTx ^. bodyTxL)) (GovActionIx 0)
    initEnactState = def & ensCurPParamsL .~ pp'
    initialGov =
      def
        & cgEnactStateL .~ initEnactState
        & cgProposalsL
          .~ fromGovActionStateSeq (SSeq.singleton $ expiringGovActionState (EpochNo 2) govActionId $ newConstitutionProposal pf)
        & cgDRepPulsingStateL .~ (DRComplete def (def & rsEnactStateL .~ initEnactState))
    initialLedgerState = LedgerState (smartUTxOState pp' utxo0 (Coin 10) (Coin 0) initialGov zero) def
    dreps = Map.singleton (drepCredential pf) (DRepState (EpochNo 2) SNothing (Coin 0))
    epochState0 =
      def
        & curPParamsEpochStateL .~ pp'
        & esLStateL .~ (initialLedgerState & lsCertStateL . certVStateL . vsDRepsL .~ dreps)
    poolDistr =
      PoolDistr
        ( Map.fromList
            [
              ( stakePoolKeyHash pf
              , IndividualPoolStake
                  spoThreshold
                  (vrfKeyHash @(EraCrypto era))
              )
            ]
        )
    assertDReps epochNo epochState =
      assertExprEqualWithMessage
        (unwords ["Epoch", show @Int epochNo, "- DReps"])
        (epochState ^. esLStateL . lsCertStateL . certVStateL . vsDRepsL)
    assertCurGovSnaps epochNo epochState expected =
      assertExprEqualWithMessage
        (unwords ["Epoch", show @Int epochNo, "CurrentProposals null status expected to be " ++ show expected ++ ".", "It was not."])
        (SSeq.null . snapshotIds $ epochState ^. esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL)
        expected
    assertPrevGovSnaps epochNo epochState expect =
      assertExprEqualWithMessage
        (unlines ["At Epoch " ++ show @Int epochNo, "PrevGovSnapshot proposals null status expected to be " ++ show expect ++ ".", "But is is not."])
        ( SSeq.null . snapshotIds $
            epochState
              ^. esLStateL
                . lsUTxOStateL
                . utxosGovStateL
                . drepPulsingStateGovStateL
                . pulsingStateSnapshotL
                . psProposalsSnapshotL
        )
        expect

    assertNumDormantEpochs epochNo prevEpochState currEpochState doesAdvance =
      assertExprEqualWithMessage
        (unwords ["Epoch", show @Int epochNo, "- NumDormantEpochs"])
        (prevEpochState ^. esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL + (if doesAdvance then 1 else 0))
        (currEpochState ^. esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL)

  assertDReps 0 epochState0 dreps
  assertCurGovSnaps 0 epochState0 False
  assertPrevGovSnaps 0 epochState0 True

  epochState1 <- expectRight "Error running runEPOCH: " $ runEPOCH (EPOCH pf) epochState0 (EpochNo 1) poolDistr

  assertNumDormantEpochs 1 epochState0 epochState1 True
  assertDReps 1 epochState1 dreps
  assertCurGovSnaps 1 epochState1 False
  assertPrevGovSnaps 1 epochState1 False

  epochState2 <- expectRight "Error running runEPOCH: " $ runEPOCH (EPOCH pf) epochState1 (EpochNo 2) poolDistr

  assertNumDormantEpochs 2 epochState1 epochState2 False
  assertDReps 2 epochState2 dreps
  assertCurGovSnaps 2 epochState2 False
  assertPrevGovSnaps 2 epochState2 False

  epochState3 <- expectRight "Error running runEPOCH: " $ runEPOCH (EPOCH pf) epochState2 (EpochNo 3) poolDistr

  assertNumDormantEpochs 3 epochState2 epochState3 False
  assertDReps 3 epochState3 dreps
  assertCurGovSnaps 3 epochState3 False
  assertPrevGovSnaps 3 epochState3 False

  epochState4 <- expectRight "Error running runEPOCH: " $ runEPOCH (EPOCH pf) epochState3 (EpochNo 4) poolDistr

  assertNumDormantEpochs 4 epochState3 epochState4 False
  assertDReps 4 epochState4 dreps
  assertCurGovSnaps 4 epochState4 True
  assertPrevGovSnaps 4 epochState4 True

  epochState5 <- expectRight "Error running runEPOCH: " $ runEPOCH (EPOCH pf) epochState4 (EpochNo 5) poolDistr

  assertNumDormantEpochs 5 epochState4 epochState5 True
  assertDReps 5 epochState5 dreps
  assertCurGovSnaps 5 epochState5 True
  assertPrevGovSnaps 5 epochState5 True

  -- Propose something
  ledgerState5 <-
    expectRight "Error running LEDGER when proposing: " $
      runLEDGER (LEDGER pf) (epochState5 ^. esLStateL) pp' (trustMeP pf True proposalTx)
  let epochState5' = epochState5 & esLStateL .~ ledgerState5
  -- Check that the vsNumDormantEpochsL has been reset to 0
  assertExprEqualWithMessage
    "Epoch 5 - NumDormantEpochs after proposal"
    (ledgerState5 ^. lsCertStateL . certVStateL . vsNumDormantEpochsL)
    0
  -- Check that the vsDRepsL <&> drepExpiryL has been bumped by the number vsNumDormantEpochsL
  assertDReps 5 epochState5' (dreps <&> drepExpiryL %~ (+ 2))

  epochState6 <- expectRight "Error running runEPOCH: " $ runEPOCH (EPOCH pf) epochState5' (EpochNo 6) poolDistr

  assertNumDormantEpochs 6 epochState5' epochState6 True
  assertDReps 6 epochState6 (dreps <&> drepExpiryL %~ (+ 2))
  assertCurGovSnaps 6 epochState6 False
  assertPrevGovSnaps 6 epochState6 False

-- ======================================================
-- Test that a vote passes
-- ======================================================

testGov ::
  forall era.
  ( State (EraRule "LEDGER" era) ~ LedgerState era
  , State (EraRule "EPOCH" era) ~ EpochState era
  , Show (PredicateFailure (EraRule "LEDGER" era))
  , Show (PredicateFailure (EraRule "EPOCH" era))
  , Scriptic era
  , GoodCrypto (EraCrypto era)
  , EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , TxCert era ~ ConwayTxCert era
  ) =>
  Proof era ->
  Assertion
testGov pf = do
  let
    (utxo0, _) = utxoFromTestCaseData pf (proposal pf)
    committee = Committee @era (Map.fromList [(ccColdCred @era pf, EpochNo 100)]) (unsafeBoundRational (1 % 2))
    committeeState = CommitteeState (Map.fromList [(ccColdCred pf, Just (ccHotCred pf))])
    initialEnactState =
      def
        & ensCurPParamsL .~ pp
        & ensCommitteeL .~ SJust committee
    initialDRepPulsingState = DRComplete def (def & rsEnactStateL .~ initialEnactState)
    initialGov =
      def
        & cgEnactStateL .~ initialEnactState
        & cgDRepPulsingStateL .~ initialDRepPulsingState
    initialLedgerState =
      LedgerState
        (smartUTxOState pp utxo0 (Coin 0) (Coin 0) initialGov zero)
        (CertState def def (def & dsUnifiedL .~ unify Map.empty Map.empty Map.empty (delegs pf)))
    initialLedgerState1 =
      initialLedgerState & lsCertStateL . certVStateL . vsCommitteeStateL .~ committeeState

    proposalTx = txFromTestCaseData pf (proposal pf)

    govActionId = GovActionId (txid (proposalTx ^. bodyTxL)) (GovActionIx 0)
    expectedGovState0 =
      fromGovActionStateSeq (SSeq.singleton $ govActionState govActionId (newConstitutionProposal pf))

    expectedGov0 =
      ConwayGovState expectedGovState0 (initialGov ^. cgEnactStateL) initialDRepPulsingState

    eitherLedgerState0 = runLEDGER (LEDGER pf) initialLedgerState1 pp (trustMeP pf True proposalTx)
  ledgerState0@(LedgerState (UTxOState _ _ _ govState0 _ _) _) <-
    expectRight "Error running LEDGER when proposing: " eitherLedgerState0

  assertExprEqualWithMessage "govState after proposal" govState0 expectedGov0

  -- Propose first constitution
  let
    voteTx = txFromTestCaseData pf (vote pf govActionId)
    gas = govActionStateWithYesVotes govActionId pf (newConstitutionProposal pf)
    expectedGovState1 = fromGovActionStateSeq $ SSeq.singleton gas
    expectedGov1 = ConwayGovState expectedGovState1 initialEnactState initialDRepPulsingState
    eitherLedgerState1 = runLEDGER (LEDGER pf) ledgerState0 pp (trustMeP pf True voteTx)
  ledgerState1@(LedgerState (UTxOState _ _ _ govState1 _ _) _) <-
    expectRight "Error running LEDGER when voting: " eitherLedgerState1

  assertExprEqualWithMessage "govState after vote" govState1 expectedGov1
  let
    epochState0 =
      (def :: EpochState era)
        & curPParamsEpochStateL .~ pp
        & esLStateL .~ ledgerState1
  let
    !poolDistr =
      PoolDistr
        ( Map.fromList
            [
              ( stakePoolKeyHash pf
              , IndividualPoolStake
                  spoThreshold
                  (vrfKeyHash @(EraCrypto era))
              )
            ]
        )
    -- Wait two epochs (one for ratification another for enactment)
    eitherEpochState1 = runEPOCH (EPOCH pf) epochState0 (EpochNo 2) poolDistr
  epochState1 <- expectRight "Error running runEPOCH: " eitherEpochState1
  let constitution = epochState1 ^. esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensConstitutionL
  assertExprEqualWithMessage "constitution after one epoch" constitution def
  let
    epochState1' = case runEPOCH (EPOCH pf) epochState1 (EpochNo 3) poolDistr of
      Left e -> error $ "Error running runEPOCH the second time: " <> show e
      Right x -> x
    ledgerState2 = epochState1' ^. esLStateL
    constitution1 = epochState1' ^. esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensConstitutionL
  assertExprEqualWithMessage "constitution after enactment" constitution1 (proposedConstitution @era)
  let
    -- Propose another constitution
    secondProposalTx = txFromTestCaseData pf (secondProposal pf govActionId)
    secondGovActionId = GovActionId (txid (secondProposalTx ^. bodyTxL)) (GovActionIx 0)
    curGAState =
      fromGovActionStateSeq . SSeq.singleton $
        govActionState secondGovActionId (anotherConstitutionProposal pf govActionId)
    expectedGovSnapshots2 = curGAState
    expectedGovState2 =
      ConwayGovState
        expectedGovSnapshots2
        (ledgerState2 ^. lsUTxOStateL . utxosGovStateL . cgEnactStateL)
        -- This might appear to be cheating, but we expect the new PulsingState to be the
        -- Completion of the one in ledgerState2, and (==) forces the competion of both its args.
        (ledgerState2 ^. lsUTxOStateL . utxosGovStateL . drepPulsingStateGovStateL)
    eitherLedgerState3 = runLEDGER (LEDGER pf) ledgerState2 pp (trustMeP pf True secondProposalTx)
  ledgerState3@(LedgerState (UTxOState _ _ _ govState2 _ _) _) <-
    expectRight "Error running LEDGER when proposing:" eitherLedgerState3

  assertExprEqualWithMessage "govState after second proposal" govState2 expectedGovState2

  -- Wait two epochs
  let
    epochState2 = epochState1' & esLStateL .~ ledgerState3
    epochState3 = case runEPOCH (EPOCH pf) epochState2 (EpochNo 4) poolDistr of
      Right x -> x
      Left e -> error $ "Error running runEPOCH: " <> show e
    epochState4 = case runEPOCH (EPOCH pf) epochState3 (EpochNo 5) poolDistr of
      Right x -> x
      Left e -> error $ "Error running runEPOCH: " <> show e
    constitution2 = epochState4 ^. esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensConstitutionL

  assertExprEqualWithMessage
    "prevGovAction set correctly"
    (SJust (PrevGovActionId govActionId))
    (epochState4 ^. esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL)
  assertExprEqualWithMessage "constitution after enactment after no votes" constitution2 (proposedConstitution @era)
  let
    currentGovActions =
      epochState4
        ^. esLStateL
          . lsUTxOStateL
          . utxosGovStateL
          . proposalsGovStateL

  case snapshotActions currentGovActions of
    gas' SSeq.:<| SSeq.Empty ->
      assertExprEqualWithMessage
        "un-enacted govAction is recorded in rsFuture"
        gas'
        ( govActionState
            secondGovActionId
            (anotherConstitutionProposal pf govActionId)
        )
    x -> error $ "Unexpected `rsFuture` after runEPOCH: " ++ show x

conwayFeatures :: TestTree
conwayFeatures =
  testGroup
    "Gov examples"
    [ testCase "Propose, vote and enact a new constitution when enough votes" $ testGov (Conway Mock)
    , testCase "Prevent DRep expiry when there are no proposals to vote on" $ preventDRepExpiry (Conway Mock)
    ]

pulsingStateSnapshotL :: Lens' (DRepPulsingState era) (PulsingSnapshot era)
pulsingStateSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter state = fst (finishDRepPulser state)
    setter (DRComplete _ y) snap = DRComplete snap y
    setter state snap = DRComplete snap $ snd $ finishDRepPulser state
