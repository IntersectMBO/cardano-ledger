{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Ratify (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), StrictMaybe (..), addEpochInterval, natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (CoinPerByte (..), ppCoinsPerUTxOByteL, ppCommitteeMinSizeL)
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
  ppCommitteeMaxTermLengthL,
  ppDRepVotingThresholdsL,
  ppPoolVotingThresholdsL,
  pvtMotionNoConfidenceL,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  epochStateGovStateL,
  esAccountStateL,
  nesELL,
  nesEsL,
 )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conformance ()
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (
  ConwayRatifyExecContext (..),
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (ExecSpecRule (..), runConformance)
import Test.Cardano.Ledger.Constrained.Conway (ConwayFn)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = describe "RATIFY" . withImpStateWithProtVer @Conway (natVersion @10) $ do
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
    treasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    let
      execCtx =
        ConwayRatifyExecContext
          treasury
          [noConfidenceGAS]
    passNEpochs 2
    getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId noConfidence)
    pure $
      testConformance @ConwayFn @"RATIFY" @Conway
        execCtx
        ratEnv
        ratSt
        (RatifySignal (noConfidenceGAS SSeq.:<| SSeq.Empty))
  it "Duplicate CC hot keys count as separate votes" $ do
    logEntry "Setting up a DRep"
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

    logEntry "Electing the committee"
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

    logEntry "Registering hotkeys"
    _ <- registerCommitteeHotKeys (pure hotKey) (ccCold0 NE.:| [ccCold1])

    logEntry "Proposing a new constitution"
    newConstitution <- arbitrary
    constitutionId <- submitGovAction $ NewConstitution SNothing newConstitution
    submitYesVote_ (CommitteeVoter hotKey) constitutionId
    submitYesVote_ (DRepVoter credDRep) constitutionId
    constitutionGAS <- getGovActionState constitutionId

    logEntry "Testing conformance"
    treasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    let execCtx = ConwayRatifyExecContext treasury [constitutionGAS]
    ratEnv <- getRatifyEnv
    govSt <- getsNES $ nesEsL . epochStateGovStateL
    let
      ratSt = getRatifyState govSt
      ratSig = RatifySignal (constitutionGAS SSeq.:<| mempty)
    (implRes, agdaRes) <-
      runConformance @"RATIFY" @ConwayFn @Conway
        execCtx
        ratEnv
        ratSt
        ratSig
    logEntry "===context==="
    logToExpr execCtx
    logEntry "===environment==="
    logToExpr ratEnv
    logEntry "===state==="
    logToExpr ratSt
    logEntry "===signal==="
    logToExpr ratSig
    logEntry "Impl res:"
    logToExpr implRes
    logEntry "Agda res:"
    logToExpr agdaRes
    logEntry "Extra information:"
    logEntry $
      extraInfo @ConwayFn @"RATIFY" @Conway
        execCtx
        ratEnv
        ratSt
        ratSig
    impAnn "Conformance failed" $ implRes `shouldBeExpr` agdaRes
