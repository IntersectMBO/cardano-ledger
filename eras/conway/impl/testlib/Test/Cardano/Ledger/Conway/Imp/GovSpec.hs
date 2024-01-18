{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ppGovActionLifetimeL, ppGovActionDepositL)
import Cardano.Ledger.Conway.Rules (
  ConwayGOV,
  ConwayGovPredFailure (..),
  ConwayLEDGER,
  ConwayLedgerPredFailure (ConwayGovFailure), PredicateFailure,
 )
import Cardano.Ledger.Core (EraRule)
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Shelley.LedgerState
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.Imp.EpochSpec (electBasicCommittee)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common hiding (Success)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (withObject, (.:), decode)
import Data.Aeson.Types (parse, Result(..))
import Cardano.Ledger.Binary (decodeHexFromText, DecCBOR, Annotator, runAnnotator, FullByteString(..))
import qualified Data.ByteString.Lazy as LBS

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , EraRule "GOV" era ~ ConwayGOV era
  , EraRule "LEDGER" era ~ ConwayLEDGER era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "GOV" $ do
    describe "Voting" $ do
      context "fails for" $ do
        it
          "committee member voting on committee change"
          committeeMemberVotingOnCommitteeChange
        it "non-committee-member voting on committee change as a committee member" $ do
          _ <- electBasicCommittee
          rewardAccount <- registerRewardAccount
          prevGovActionId <- getsNES $
            nesEsL .
            esLStateL .
            lsUTxOStateL .
            utxosGovStateL .
            cgEnactStateL .
            ensPrevGovActionIdsL .
            pgaCommitteeL
          credCandidate <- KeyHashObj <$> freshKeyHash
          credVoter <- KeyHashObj <$> freshKeyHash
          govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
          committeeUpdateId <- submitProposal ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = UpdateCommittee
                prevGovActionId
                mempty
                (Map.singleton credCandidate $ EpochNo 28)
                (3 %! 5)
            , pProcDeposit = govActionDeposit
            , pProcAnchor = def
            }
          let voter = CommitteeVoter credVoter
          trySubmitVote VoteNo voter committeeUpdateId
            `shouldReturn`
              Left
                [ ConwayGovFailure . DisallowedVoters @era $
                    Map.fromList
                      [ (committeeUpdateId, voter)
                      ]
                ]
    describe "Votes fail as expected" $ do
      context "Invalid proposals are rejected" $ do
        it "Enact Constitution and use valid PrevGovActionId" $ do
          (dRep, committeeMember) <- electBasicCommittee
          constitutionHash <- freshSafeHash
          let constitution =
                Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash
                  )
                  SNothing
              constitutionAction =
                NewConstitution SNothing constitution
          gaidConstitutionProp <- submitGovAction constitutionAction
          submitYesVote_ (DRepVoter dRep) gaidConstitutionProp
          submitYesVote_ (CommitteeVoter committeeMember) gaidConstitutionProp
          passEpoch
          passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          impAnn "Constitution has been enacted" $
            curConstitution `shouldBe` constitution
          constitutionHash1 <- freshSafeHash
          let constitutionAction1 =
                NewConstitution
                  (SJust $ PrevGovActionId gaidConstitutionProp)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl "constitution.1")
                          constitutionHash1
                      )
                      SNothing
                  )
          submitGovAction_ constitutionAction1

committeeMemberVotingOnCommitteeChange ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
committeeMemberVotingOnCommitteeChange = do
  (_, ccHot) <- electBasicCommittee
  rewardAccount <- registerRewardAccount
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  khCommittee <- KeyHashObj <$> freshKeyHash
  prevGovActionId <- getsNES $
    nesEsL .
    esLStateL .
    lsUTxOStateL .
    utxosGovStateL .
    cgEnactStateL .
    ensPrevGovActionIdsL .
    pgaCommitteeL
  committeeUpdateId <- submitProposal ProposalProcedure
    { pProcReturnAddr = rewardAccount
    , pProcGovAction = UpdateCommittee
        prevGovActionId
        mempty
        (Map.singleton khCommittee $ EpochNo 28)
        (3 %! 5)
    , pProcDeposit = govActionDeposit
    , pProcAnchor = def
    }
  let voter = CommitteeVoter ccHot
  submitFailingVote
    voter
    committeeUpdateId
    [ ConwayGovFailure . DisallowedVoters @era $
        Map.fromList
          [ (committeeUpdateId, voter)
          ]
    ]
