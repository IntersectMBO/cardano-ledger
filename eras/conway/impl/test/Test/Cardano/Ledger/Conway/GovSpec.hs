{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GovSpec where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Shelley.LedgerState
import Data.Default.Class (Default (..))
import Data.Maybe
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.ImpTest

spec :: Spec
spec =
  describe "Proposals always have valid previous actions" $ do
    context "Invalid proposals are rejected" $ do
      itM @Conway "Lookup snapshots" $ do
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
                (SJust $ PrevGovActionId (gaidConstitutionProp {gaidGovActionIx = GovActionIx 1})) -- make it wrong
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.1")
                        constitutionHash
                    )
                    SNothing
                )
        submitFailingProposal constitutionActionNext
      itM @Conway "Lookup snapshots" $ do
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
              .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add the wrong one to PrevGovActionIds
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgGovSnapshotsL
              .~ def -- Remove from snapshots
        let constitutionActionNext =
              NewConstitution
                (SJust $ PrevGovActionId (gaidConstitutionProp {gaidGovActionIx = GovActionIx 1})) -- make it wrong
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.1")
                        constitutionHash
                    )
                    SNothing
                )
        submitFailingProposal constitutionActionNext
    context "Valid proposals are accepted" $ do
      itM @Conway "Lookup snapshots" $ do
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
      itM @Conway "Lookup PrevGovActionIds" $ do
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
              .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add to PrevGovActionIds
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgGovSnapshotsL
              .~ def -- Remove from snapshots
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
