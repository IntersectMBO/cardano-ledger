{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovCertSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Shelley.LedgerState
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common hiding (Success)

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "GOVCERT" $ do
    it "A CC that has resigned will need to be first voted out and then voted in to be considered active" $ do
      (dRep, _, gaidCC) <- electBasicCommittee
      passNEpochs 2
      -- Add a fresh CC
      cc <- KeyHashObj <$> freshKeyHash
      let addCCAction = UpdateCommittee (SJust gaidCC) mempty (Map.singleton cc 20) (1 %! 2)
      addCCGaid <- submitGovAction addCCAction
      submitYesVote_ (DRepVoter dRep) addCCGaid
      passNEpochs 2
      -- Confirm that they are added
      SJust committee <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      let assertCCMembership comm =
            assertBool "Expected CC to be present in the committee" $
              Map.member cc (comm ^. committeeMembersL)
          assertCCMissing comm =
            assertBool "Expected CC to be absent in the committee" $
              Map.notMember cc (comm ^. committeeMembersL)
      assertCCMembership committee
      -- Confirm their hot key registration
      _hotKey <- registerCommitteeHotKey cc
      ccShouldNotBeResigned cc
      -- Have them resign
      resignCommitteeColdKey cc
      ccShouldBeResigned cc
      -- Re-add the same CC
      let reAddCCAction = UpdateCommittee (SJust $ GovPurposeId addCCGaid) mempty (Map.singleton cc 20) (1 %! 2)
      reAddCCGaid <- submitGovAction reAddCCAction
      submitYesVote_ (DRepVoter dRep) reAddCCGaid
      passNEpochs 2
      -- Confirm that they are still resigned
      ccShouldBeResigned cc
      -- Remove them
      let removeCCAction = UpdateCommittee (SJust $ GovPurposeId reAddCCGaid) (Set.singleton cc) mempty (1 %! 2)
      removeCCGaid <- submitGovAction removeCCAction
      submitYesVote_ (DRepVoter dRep) removeCCGaid
      passNEpochs 2
      -- Confirm that they have been removed
      SJust committeeAfterRemove <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      assertCCMissing committeeAfterRemove
      -- Add the same CC back a second time
      let secondAddCCAction = UpdateCommittee (SJust $ GovPurposeId removeCCGaid) mempty (Map.singleton cc 20) (1 %! 2)
      secondAddCCGaid <- submitGovAction secondAddCCAction
      submitYesVote_ (DRepVoter dRep) secondAddCCGaid
      passNEpochs 2
      -- Confirm that they have been added
      SJust committeeAfterRemoveAndAdd <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      assertCCMembership committeeAfterRemoveAndAdd
      -- Confirm that after registering a hot key, they are active
      _hotKey <- registerCommitteeHotKey cc
      ccShouldNotBeResigned cc
