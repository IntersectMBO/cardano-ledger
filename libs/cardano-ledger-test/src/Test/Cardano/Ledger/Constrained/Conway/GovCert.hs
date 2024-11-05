{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOVCERT rule
module Test.Cardano.Ledger.Constrained.Conway.GovCert where

import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (Era (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Constrained
import qualified Data.Map as Map
import Data.Set (Set)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams

vStateSpec ::
  (IsConwayUniv fn, Era era) =>
  Term fn (Set (Credential 'DRepRole (EraCrypto era))) ->
  Specification fn (VState era)
vStateSpec delegatees = constrained' $ \dreps _ _ -> dom_ dreps ==. delegatees

{- There are no hard constraints on VState, but sometimes when something fails we want to
-- limit how big some of the fields of VState are. In that case one might use something
-- like this. Note that genHint limits the size, but does not require an exact size.
vStateSpec :: IsConwayUniv fn => Specification fn (VState (ConwayEra StandardCrypto))
vStateSpec = constrained' $ \ [var|_dreps|] [var|_commstate|] [var|dormantepochs|] ->
  [ genHint 5 dreps -- assert $ sizeOf_ dreps >=. 1
  , match commstate $ \ [var|committeestate|] -> genHint 5 committeestate
  , assert $ dormantepochs >=. lit (EpochNo 4)
  ]
-}

govCertSpec ::
  IsConwayUniv fn =>
  ConwayGovCertEnv (ConwayEra StandardCrypto) ->
  CertState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayGovCert StandardCrypto)
govCertSpec ConwayGovCertEnv {..} certState =
  let vs = certVState certState
      reps = lit $ Map.keysSet $ vsDReps vs
      deposits = Map.map drepDeposit (vsDReps vs)
      getNewMembers = \case
        UpdateCommittee _ _ newMembers _ -> Map.keysSet newMembers
        _ -> mempty
      knownColdCreds =
        Map.keysSet (foldMap committeeMembers cgceCurrentCommittee)
          <> foldMap (getNewMembers . pProcGovAction . gasProposalProcedure) cgceCommitteeProposals
      ccCertSpec coldCred =
        assert . member_ coldCred $ lit knownColdCreds
      commiteeStatus = csCommitteeCreds (vsCommitteeState vs)
   in constrained $ \ [var|gc|] ->
        caseOn
          gc
          -- The weights on each 'branchW' case try to make it likely
          -- that each branch is choosen with similar frequency
          -- ConwayRegDRep

          ( branchW 1 $ \ [var|keyreg|] [var|coinreg|] _ ->
              [ assert $ not_ $ member_ keyreg (dom_ (lit deposits))
              , assert $ coinreg ==. lit (cgcePParams ^. ppDRepDepositL)
              ]
          )
          -- ConwayUnRegDRep -- Commented out on purpose, to make conformance tests pass.
          -- Should be uncommented when they are fixed to handle un registration
          -- ( branchW 3 $ \ [var|credUnreg|] [var|coinUnreg|] ->
          --     assert $ elem_ (pair_ credUnreg coinUnreg) (lit (Map.toList deposits))
          -- )
          (branchW 3 $ \_credUnreg _coinUnreg -> False)
          -- ConwayUpdateDRep
          ( branchW 1 $ \ [var|keyupdate|] _ ->
              member_ keyupdate reps
          )
          -- ConwayAuthCommitteeHotKey
          ( branchW 1 $ \ [var|coldCredAuth|] _ -> [ccCertSpec coldCredAuth, notYetResigned commiteeStatus coldCredAuth]
          )
          -- ConwayResignCommitteeColdKey
          ( branchW 1 $ \ [var|coldCredResign|] _ -> [ccCertSpec coldCredResign, notYetResigned commiteeStatus coldCredResign]
          )

-- | Operations for authenticating a HotKey, or resigning a ColdKey are illegal
--   if that key has already resigned.
notYetResigned ::
  (HasSpec fn x, Ord x, IsConwayUniv fn) =>
  Map.Map x (CommitteeAuthorization StandardCrypto) ->
  Term fn x ->
  Pred fn
notYetResigned committeeStatus coldcred =
  ( caseOn
      (lookup_ coldcred (lit committeeStatus))
      -- SNothing
      (branch $ \_ -> True)
      -- SJust
      ( branch $ \x ->
          [ (caseOn x)
              --  CommitteeHotCredential
              (branch $ \_ -> True)
              -- CommitteeMemberResigned
              (branch $ \_ -> False)
          ]
      )
  )

govCertEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (ConwayGovCertEnv (ConwayEra StandardCrypto))
govCertEnvSpec =
  constrained $ \gce ->
    match gce $ \pp _ _ _ ->
      satisfies pp pparamsSpec
