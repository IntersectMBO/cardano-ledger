{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovSpec where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ppGovActionLifetimeL)
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure (GovActionsDoNotExist, VotingOnExpiredGovAction),
  ConwayLedgerPredFailure (ConwayGovFailure),
 )
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Shelley.LedgerState
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.ImpTest

spec :: Spec
spec = do
  describe "Votes fail as expected" $ do
    itM @Conway "on expired gov-actions" $ do
      modifyPParams $ ppGovActionLifetimeL .~ 2 -- Voting after the 3rd epoch should fail
      khDRep <- setupSingleDRep
      constitutionHash <- freshSafeHash
      let constitutionAction =
            NewConstitution
              SNothing
              ( Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash
                  )
                  SNothing
              )
      gaidConstitutionProp <- submitProposal constitutionAction
      passEpoch
      passEpoch
      passEpoch
      let voter = DRepVoter $ KeyHashObj khDRep
      tryVoteForProposal voter gaidConstitutionProp
        >>= predicateFailureShouldBe @"LEDGER" @Conway
          ( ConwayGovFailure $
              VotingOnExpiredGovAction $
                Map.singleton gaidConstitutionProp voter
          )
    itM @Conway "on non-existant gov-actions" $ do
      khDRep <- setupSingleDRep
      constitutionHash <- freshSafeHash
      let constitutionAction =
            NewConstitution
              SNothing
              ( Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash
                  )
                  SNothing
              )
      gaidConstitutionProp <- submitProposal constitutionAction
      let voter = DRepVoter $ KeyHashObj khDRep
          dummyGaid = gaidConstitutionProp {gaidGovActionIx = GovActionIx 99} -- non-existant `GovActioNId`
      tryVoteForProposal voter dummyGaid
        >>= predicateFailureShouldBe @"LEDGER" @Conway
          ( ConwayGovFailure $
              GovActionsDoNotExist $
                Set.singleton dummyGaid
          )

  describe "Proposals always have valid previous actions" $ do
    itM @Conway "First ever proposal is accepted without needing a PrevGovActionId but after it is enacted, the following ones are not" $ do
      constitutionHash <- freshSafeHash
      let constitutionAction =
            NewConstitution
              SNothing
              ( Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash
                  )
                  SNothing
              )
      gaidConstitutionProp <- submitProposal constitutionAction
      constitutionHash' <- freshSafeHash
      let constitutionAction' =
            NewConstitution
              SNothing
              ( Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash'
                  )
                  SNothing
              )
      -- Until the first proposal is enacted all proposals with empty PrevGovActionIds are valid
      _gaidConstitutionProp' <- submitProposal constitutionAction'
      modifyNES $ \nes ->
        nes
          & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
            .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add first proposal to PrevGovActionIds in enacted state
          & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
            .~ def -- Remove all proposals from snapshots, so that the lookup only succeeds for enacted state
      constitutionHash'' <- freshSafeHash
      -- Once a proposal with a purpose has been enacted, following proposals can no longer have empty PrevGovActionIds
      let constitutionAction'' =
            NewConstitution
              SNothing
              ( Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash''
                  )
                  SNothing
              )
      submitFailingProposal constitutionAction''
    context "Invalid proposals are rejected" $ do
      itM @Conway "Lookup in snapshots for invalid PrevGovActionId" $ do
        constitutionHash <- freshSafeHash
        let constitutionAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash
                    )
                    SNothing
                )
        gaidConstitutionProp <- submitProposal constitutionAction
        let constitutionActionNext =
              NewConstitution
                (SJust $ PrevGovActionId (gaidConstitutionProp {gaidGovActionIx = GovActionIx 1})) -- Expected Ix = 0
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.1")
                        constitutionHash
                    )
                    SNothing
                )
        submitFailingProposal constitutionActionNext
      itM @Conway "Lookup in snapshots for valid PrevGovActionId but invalid purpose" $ do
        constitutionHash <- freshSafeHash
        let constitutionAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash
                    )
                    SNothing
                )
        gaidConstitutionProp <- submitProposal constitutionAction
        let noConfidenceAction =
              NoConfidence $ SJust $ PrevGovActionId gaidConstitutionProp
        submitFailingProposal noConfidenceAction
      itM @Conway "Lookup enacted state for invalid PrevGovActionId" $ do
        constitutionHash <- freshSafeHash
        let constitutionAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash
                    )
                    SNothing
                )
        gaidConstitutionProp <- submitProposal constitutionAction
        modifyNES $ \nes ->
          nes
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
              .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add it to PrevGovActionIds in enacted state
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
              .~ def -- Remove from snapshots, so that the lookup only succeeds for enacted state
        let constitutionActionNext =
              NewConstitution
                (SJust $ PrevGovActionId (gaidConstitutionProp {gaidGovActionIx = GovActionIx 1})) -- Expected Ix = 0
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.1")
                        constitutionHash
                    )
                    SNothing
                )
        submitFailingProposal constitutionActionNext
      itM @Conway "Lookup enacted state for valid PrevGovActionId but invalid purpose" $ do
        constitutionHash <- freshSafeHash
        let constitutionAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash
                    )
                    SNothing
                )
        gaidConstitutionProp <- submitProposal constitutionAction
        modifyNES $ \nes ->
          nes
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
              .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add it to PrevGovActionIds in enacted state
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
              .~ def -- Remove from snapshots, so that the lookup only succeeds for enacted state
        let noConfidenceAction =
              NoConfidence $ SJust $ PrevGovActionId gaidConstitutionProp
        submitFailingProposal noConfidenceAction
    context "Valid proposals are accepted" $ do
      itM @Conway "Lookup snapshots for valid PrevGovActionId and purpose" $ do
        constitutionHash <- freshSafeHash
        let constitutionAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash
                    )
                    SNothing
                )
        gaidConstitutionProp <- submitProposal constitutionAction
        let constitutionActionNext =
              NewConstitution
                (SJust $ PrevGovActionId gaidConstitutionProp)
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.1")
                        constitutionHash
                    )
                    SNothing
                )
        submitProposal constitutionActionNext
      itM @Conway "Lookup enacted state for valid PrevGovActionId and purpose" $ do
        constitutionHash <- freshSafeHash
        let constitutionAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash
                    )
                    SNothing
                )
        gaidConstitutionProp <- submitProposal constitutionAction
        modifyNES $ \nes ->
          nes
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
              .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add it to PrevGovActionIds in enacted state
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
              .~ def -- Remove from snapshots, so that the lookup only succeeds for enacted state
        let constitutionActionNext =
              NewConstitution
                (SJust $ PrevGovActionId gaidConstitutionProp)
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.1")
                        constitutionHash
                    )
                    SNothing
                )
        submitProposal constitutionActionNext
