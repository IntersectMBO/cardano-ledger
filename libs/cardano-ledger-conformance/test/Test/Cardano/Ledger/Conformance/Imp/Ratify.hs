{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Ratify (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), StrictMaybe (..), addEpochInterval)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  GovAction (..),
  GovPurposeId (..),
  RatifySignal (..),
  Voter (..),
  committeeGovStateL,
  getRatifyState,
 )
import Cardano.Ledger.Conway.PParams (
  dvtMotionNoConfidenceL,
  pvtMotionNoConfidenceL,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (
  epochStateGovStateL,
  esChainAccountStateL,
  nesELL,
  nesEsL,
 )
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Conformance (showOpaqueErrorString)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (
  ConwayRatifyExecContext (..),
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (ExecSpecRule (..), runConformance)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = withImpInit @(LedgerSpec ConwayEra) $ describe "RATIFY" $ modifyImpInitProtVer (eraProtVerHigh @ConwayEra) $ do
  it "NoConfidence accepted conforms" $ do
    modifyPParams $ \pp ->
      pp
        & ppDRepVotingThresholdsL . dvtMotionNoConfidenceL .~ 9 %! 10
        & ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 0 %! 1
    (dRep, _, _) <- electBasicCommittee
    lastCommittee <- getLastEnactedCommittee
    noConfidence <- submitGovAction $ NoConfidence lastCommittee
    submitYesVote_ (DRepVoter dRep) noConfidence
    ratEnv <- getRatifyEnv
    govSt <- getsNES $ nesEsL . epochStateGovStateL
    let ratSt = getRatifyState govSt
    noConfidenceGAS <- getGovActionState noConfidence
    treasury <- getsNES $ nesEsL . esChainAccountStateL . casTreasuryL
    let
      execCtx =
        ConwayRatifyExecContext
          treasury
          [noConfidenceGAS]
    passNEpochs 2
    getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId noConfidence)
    pure $
      testConformance @"RATIFY" @ConwayEra
        execCtx
        ratEnv
        ratSt
        (RatifySignal (noConfidenceGAS SSeq.:<| SSeq.Empty))
  it "Duplicate CC hot keys count as separate votes" $ do
    logString "Setting up a DRep"
    let maxTermLength = EpochInterval 10
    modifyPParams $ \pp ->
      pp
        & ppCommitteeMaxTermLengthL .~ maxTermLength
        & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1)
        & ppCommitteeMinSizeL .~ 2
    (credDRep, _, _) <- setupSingleDRep 300
    (credSPO, _, _) <- setupPoolWithStake $ Coin 1_000_000
    -- Ensure that there is no committee yet
    SJust (Committee {committeeMembers = oldCommittee}) <-
      getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL

    logString "Electing the committee"
    ccCold0 <- KeyHashObj <$> freshKeyHash
    ccCold1 <- KeyHashObj <$> freshKeyHash
    ccCold2 <- KeyHashObj <$> freshKeyHash
    hotKey <- KeyHashObj <$> freshKeyHash
    curEpoch <- getsNES nesELL
    let
      ccExpiry = curEpoch `addEpochInterval` maxTermLength
      committee =
        Map.fromList
          [ (ccCold0, ccExpiry)
          , (ccCold1, ccExpiry)
          , (ccCold2, ccExpiry)
          ]
    committeeId <-
      submitGovAction $
        UpdateCommittee
          SNothing
          (Map.keysSet oldCommittee) -- Get rid of the initial committee
          committee
          (6 %! 10)
    submitYesVote_ (DRepVoter credDRep) committeeId
    submitYesVote_ (StakePoolVoter credSPO) committeeId
    impAnn "Waiting for the committee to get elected" $ do
      logAcceptedRatio committeeId
      passNEpochs 2
      getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId committeeId)

    logString "Registering hotkeys"
    _ <- registerCommitteeHotKeys (pure hotKey) (ccCold0 NE.:| [ccCold1])

    logString "Proposing a new constitution"
    newConstitution <- arbitrary
    constitutionId <- submitGovAction $ NewConstitution SNothing newConstitution
    submitYesVote_ (CommitteeVoter hotKey) constitutionId
    submitYesVote_ (DRepVoter credDRep) constitutionId
    constitutionGAS <- getGovActionState constitutionId

    logString "Testing conformance"
    treasury <- getsNES $ nesEsL . esChainAccountStateL . casTreasuryL
    let execCtx = ConwayRatifyExecContext treasury [constitutionGAS]
    ratEnv <- getRatifyEnv
    govSt <- getsNES $ nesEsL . epochStateGovStateL
    let
      ratSt = getRatifyState govSt
      ratSig = RatifySignal (constitutionGAS SSeq.:<| mempty)
    (implRes, agdaRes, implRes') <-
      runConformance @"RATIFY" @ConwayEra
        execCtx
        ratEnv
        ratSt
        ratSig
    logString "===context==="
    logToExpr execCtx
    logString "===environment==="
    logToExpr ratEnv
    logString "===state==="
    logToExpr ratSt
    logString "===signal==="
    logToExpr ratSig
    logString "Impl res:"
    logToExpr implRes
    logString "Agda res:"
    logToExpr agdaRes
    logString "Extra information:"
    globals <- use impGlobalsL
    logDoc $
      extraInfo @"RATIFY" @ConwayEra
        globals
        execCtx
        ratEnv
        ratSt
        ratSig
        (first showOpaqueErrorString implRes')
    impAnn "Conformance failed" $
      first showOpaqueErrorString implRes `shouldBeExpr` agdaRes
