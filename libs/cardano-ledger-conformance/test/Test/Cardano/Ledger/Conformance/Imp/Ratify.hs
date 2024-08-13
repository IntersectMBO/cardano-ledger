{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Ratify (spec) where

import qualified Data.List.NonEmpty as NE
import Cardano.Ledger.BaseTypes (StrictMaybe (..), natVersion, addEpochInterval, EpochInterval (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovPurposeId (..),
  RatifySignal (..),
  Voter (..),
  getRatifyState,
 )
import Cardano.Ledger.Conway.PParams (
  dvtMotionNoConfidenceL,
  ppDRepVotingThresholdsL,
  ppPoolVotingThresholdsL,
  pvtMotionNoConfidenceL, ppCommitteeMaxTermLengthL,
 )
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  epochStateGovStateL,
  esAccountStateL,
  nesEsL,
  nesELL,
 )
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (
  ConwayRatifyExecContext (..),
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (ExecSpecRule (..))
import Test.Cardano.Ledger.Constrained.Conway (ConwayFn)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common
import Cardano.Ledger.Credential (Credential(..))
import qualified Data.Map as Map

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
    let maxTermLength = EpochInterval 10
    modifyPParams $ \pp ->
      pp
        & ppCommitteeMaxTermLengthL .~ maxTermLength
    (credDRep, _, _) <- setupSingleDRep 100
    ccCold0 <- KeyHashObj <$> freshKeyHash
    ccCold1 <- KeyHashObj <$> freshKeyHash
    hotKey <- KeyHashObj <$> freshKeyHash
    curEpoch <- getsNES nesELL
    let
      ccExpiry = curEpoch `addEpochInterval` maxTermLength
      committee = Map.fromList
        [ (ccCold0, ccExpiry)
        , (ccCold1, ccExpiry)
        ]

    logEntry "Electing the committee"
    committeeId <- submitGovAction $ UpdateCommittee SNothing mempty committee maxBound
    submitYesVote_ (DRepVoter credDRep) committeeId
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
      result =
        testConformance @ConwayFn @"RATIFY" @Conway
          execCtx
          ratEnv
          ratSt
          (RatifySignal (constitutionGAS SSeq.:<| mempty))
    pure result
