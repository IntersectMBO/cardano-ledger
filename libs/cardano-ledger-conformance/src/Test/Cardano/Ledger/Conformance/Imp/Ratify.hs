{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.Imp.Ratify (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..), natVersion)
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
  pvtMotionNoConfidenceL,
 )
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  epochStateGovStateL,
  esAccountStateL,
  nesEsL,
 )
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Sequence.Strict as SSeq
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway ()
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (ConwayRatifyExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (ExecSpecRule (..))
import Test.Cardano.Ledger.Constrained.Conway (ConwayFn)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = describe "RATIFY" . withImpStateWithProtVer @Conway (natVersion @10) $
  it "NoConfidence accepted conforms" $
    do
      (dRep, _, _) <- electBasicCommittee
      modifyPParams $ \pp ->
        pp
          & ppDRepVotingThresholdsL . dvtMotionNoConfidenceL .~ 9 %! 10
          & ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 0 %! 1
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
      let
        args =
          Args
            { replay = Nothing
            , maxSuccess = 1
            , maxSize = 0
            , maxShrinks = 0
            , maxDiscardRatio = 0
            , chatty = False
            }
      passEpoch
      -- TODO figure out a less hacky solution
      liftIO . quickCheckWith args $
        testConformance @ConwayFn @"RATIFY" @Conway
          execCtx
          ratEnv
          ratSt
          (RatifySignal (noConfidenceGAS SSeq.:<| SSeq.Empty))
      passEpoch
      getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId noConfidence)
