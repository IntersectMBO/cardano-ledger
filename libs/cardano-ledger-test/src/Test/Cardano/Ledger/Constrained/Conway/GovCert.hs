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
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained
import qualified Data.Map as Map
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams

vStateSpec :: Specification fn (VState (ConwayEra StandardCrypto))
vStateSpec = TrueSpec

govCertSpec ::
  IsConwayUniv fn =>
  ConwayGovCertEnv (ConwayEra StandardCrypto) ->
  VState (ConwayEra StandardCrypto) ->
  Specification fn (ConwayGovCert StandardCrypto)
govCertSpec ConwayGovCertEnv {..} vs =
  let reps = lit $ Map.keysSet $ vsDReps vs
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
          -- ConwayUnRegDRep
          ( branchW 3 $ \ [var|credUnreg|] [var|coinUnreg|] ->
              assert $ elem_ (pair_ credUnreg coinUnreg) (lit (Map.toList deposits))
          )
          -- ConwayUpdateDRep
          ( branchW 1 $ \keyupdate _ ->
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
