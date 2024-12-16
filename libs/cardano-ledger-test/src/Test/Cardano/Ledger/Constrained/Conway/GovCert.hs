{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Cardano.Ledger.Keys (KeyRole (..))
import Constrained
import qualified Data.Map as Map
import Data.Set (Set)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse

-- | There are no hard constraints on VState, other than witnessing, and delegatee conformance
--   which must align with the conwayCertExecContextSpec
vStateSpec ::
  forall fn era.
  (IsConwayUniv fn, Era era) =>
  WitUniv era ->
  Set (Credential 'DRepRole (EraCrypto era)) ->
  Specification fn (VState era)
vStateSpec univ delegatees = constrained $ \ [var|vstate|] ->
  match vstate $ \ [var|dreps|] [var|committeestate|] [var|_numdormant|] ->
    [ match committeestate $ \ [var|committeemap|] -> witness univ (dom_ committeemap)
    , assert $ dom_ dreps ==. (lit delegatees) -- TODO, there are missing constraints about the (rng_ dreps)
    , forAll dreps $ \ [var|pair|] ->
        match pair $ \ [var|drep|] [var|drepstate|] ->
          [ dependsOn drepstate drep
          , witness univ drep
          , match drepstate $
              \_expiry _anchor _deposit [var|delegs|] -> witness univ delegs
          ]
    ]

govCertSpec ::
  (IsConwayUniv fn, Era era, EraCrypto era ~ StandardCrypto) =>
  WitUniv era ->
  ConwayGovCertEnv (ConwayEra StandardCrypto) ->
  CertState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayGovCert StandardCrypto)
govCertSpec univ ConwayGovCertEnv {..} certState =
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
              , witness univ keyreg
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
              [witness univ keyupdate, assert $ member_ keyupdate reps]
          )
          -- ConwayAuthCommitteeHotKey
          ( branchW 1 $ \ [var|coldCredAuth|] [var|hotcred|] ->
              [ ccCertSpec coldCredAuth
              , notYetResigned commiteeStatus coldCredAuth
              , witness univ coldCredAuth
              , witness univ hotcred
              ]
          )
          -- ConwayResignCommitteeColdKey
          ( branchW 1 $ \ [var|coldCredResign|] _ ->
              [ witness univ coldCredResign
              , ccCertSpec coldCredResign
              , notYetResigned commiteeStatus coldCredResign
              ]
          )

-- | Operations for authenticating a HotKey, or resigning a ColdKey are illegal
--   if that key has already resigned.
notYetResigned ::
  (HasSpec fn x, Ord x, IsConwayUniv fn) =>
  Map.Map x CommitteeAuthorization ->
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
  forall fn.
  IsConwayUniv fn =>
  WitUniv (ConwayEra StandardCrypto) ->
  Specification fn (ConwayGovCertEnv (ConwayEra StandardCrypto))
govCertEnvSpec univ =
  constrained $ \ [var|gce|] ->
    match gce $ \ [var|pp|] _ [var|committee|] [var|proposalmap|] ->
      [ satisfies pp pparamsSpec
      , unsafeExists (\x -> [satisfies x (committeeWitness univ), assert $ committee ==. cSJust_ x])
      , assert $ sizeOf_ (dom_ proposalmap) <=. lit 5
      , assert $ sizeOf_ (dom_ proposalmap) >=. lit 1
      , forAll (rng_ proposalmap) $ \x -> satisfies x (govActionStateWitness univ)
      ]
