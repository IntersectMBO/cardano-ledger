{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERT rule
module Test.Cardano.Ledger.Constrained.Conway.Cert where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (KeyRoleVRF (GenDelegVRF), VRFVerKeyHash)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert (..))
import Constrained
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Constrained.Conway.Pool
import Test.QuickCheck hiding (forAll)

certEnvSpec ::
  forall fn era.
  (EraSpecPParams era, IsConwayUniv fn) =>
  Specification fn (CertEnv era)
certEnvSpec =
  constrained $ \ce ->
    match ce $ \pp _currEpoch _currCommittee _proposals ->
      [ satisfies pp pparamsSpec
      ]

certStateSpec ::
  (IsConwayUniv fn, EraSpecDeleg era) =>
  Term fn (Set (Credential 'DRepRole)) ->
  Specification fn (CertState era)
certStateSpec delegatees =
  constrained $ \cs ->
    match cs $ \vState pState dState ->
      [ satisfies vState (vStateSpec delegatees)
      , satisfies pState pStateSpec
      , satisfies dState dStateSpec
      ]

certStateSpecEx ::
  (IsConwayUniv fn, EraSpecDeleg era) =>
  Specification fn (CertState era)
certStateSpecEx = constrained $ \st ->
  exists
    (\eval -> pure . Map.keysSet . vsDReps . certVState $ eval st)
    (\delegatees -> st `satisfies` certStateSpec delegatees)

conwayTxCertSpec ::
  IsConwayUniv fn =>
  CertEnv ConwayEra ->
  CertState ConwayEra ->
  Specification fn (ConwayTxCert ConwayEra)
conwayTxCertSpec (CertEnv pp ce cc cp) certState@CertState {..} =
  constrained $ \txCert ->
    caseOn
      txCert
      -- These weights try to make it equally likely that each of the many certs
      -- across the 3 categories are chosen at similar frequencies.
      (branchW 3 $ \delegCert -> satisfies delegCert $ conwayDelegCertSpec delegEnv certState)
      (branchW 1 $ \poolCert -> satisfies poolCert $ poolCertSpec poolEnv certPState)
      (branchW 2 $ \govCert -> satisfies govCert $ govCertSpec govCertEnv certState)
  where
    delegEnv = ConwayDelegEnv pp (psStakePoolParams certPState)
    poolEnv = PoolEnv ce pp
    govCertEnv = ConwayGovCertEnv pp ce cc cp

-- ==============================================================
-- Shelley Certs

-- | Genesis delegations only work through the Babbage era. Hence the (AtMostEra BabbageEra era)
genesisDelegCertSpec ::
  forall fn era.
  (AtMostEra BabbageEra era, IsConwayUniv fn, Era era) =>
  DState era -> Specification fn GenesisDelegCert
genesisDelegCertSpec ds =
  let (vrfKeyHashes, coldKeyHashes) = computeSets ds
      GenDelegs genDelegs = dsGenDelegs ds
   in constrained $ \ [var|gdc|] ->
        match gdc $ \ [var|gkh|] [var|vkh|] [var|hashVrf|] ->
          [ assert $ member_ gkh (dom_ (lit genDelegs))
          , reify gkh coldKeyHashes (\ [var|coldkeys|] -> member_ vkh coldkeys)
          , reify gkh vrfKeyHashes (\ [var|vrfkeys|] -> member_ hashVrf vrfkeys)
          ]

-- | Compute 2 functions from the DState. Each function, given a KeyHash,
--   returns a Set of 'Hashes', we expect certain things to be in those sets.
--   This mimics what happens in the Cardano.Ledger.Shelley.Rules.Deleg module
computeSets ::
  DState era ->
  ( KeyHash 'Genesis -> Set (VRFVerKeyHash 'GenDelegVRF)
  , KeyHash 'Genesis -> Set (KeyHash 'GenesisDelegate)
  )
computeSets ds =
  let genDelegs = unGenDelegs (dsGenDelegs ds)
      futureGenDelegs = dsFutureGenDelegs ds
      cod gkh = Set.fromList $ Map.elems $ Map.delete gkh genDelegs
      fod gkh =
        Set.fromList $ Map.elems $ Map.filterWithKey (\(FutureGenDeleg _ g) _ -> g /= gkh) futureGenDelegs
      currentColdKeyHashes gkh = Set.map genDelegKeyHash (cod gkh)
      currentVrfKeyHashes gkh = Set.map genDelegVrfHash (cod gkh)
      futureColdKeyHashes gkh = Set.map genDelegKeyHash (fod gkh)
      futureVrfKeyHashes gkh = Set.map genDelegVrfHash (fod gkh)
      coldKeyHashes gkh = currentColdKeyHashes gkh <> futureColdKeyHashes gkh
      vrfKeyHashes gkh = currentVrfKeyHashes gkh <> futureVrfKeyHashes gkh
   in (vrfKeyHashes, coldKeyHashes)

-- =======================================

shelleyTxCertSpec ::
  forall fn era.
  (AtMostEra BabbageEra era, EraSpecPParams era, IsConwayUniv fn) =>
  CertEnv era ->
  CertState era ->
  Specification fn (ShelleyTxCert era)
shelleyTxCertSpec (CertEnv pp e _ _) (CertState _vstate pstate dstate) =
  constrained $ \ [var|shelleyTxCert|] ->
    -- These weights try to make it equally likely that each of the many certs
    -- across the 3 categories are chosen at similar frequencies.
    (caseOn shelleyTxCert)
      ( branchW 5 $ \ [var|deleg|] ->
          satisfies
            deleg
            ( shelleyDelegCertSpec @fn @era
                (ConwayDelegEnv pp (psStakePoolParams pstate))
                dstate
            )
      )
      (branchW 3 $ \ [var|poolCert|] -> satisfies poolCert $ poolCertSpec (PoolEnv e pp) pstate)
      (branchW 1 $ \ [var|genesis|] -> satisfies genesis (genesisDelegCertSpec @fn @era dstate))
      (branchW 1 $ \ [var|_mir|] -> False) -- By design, we never generate a MIR cert

-- =========================================================================
-- Making Cert Era parametric with the EraSpecCert class

class
  ( Era era
  , IsConwayUniv fn
  , HasSpec fn (TxCert era)
  ) =>
  EraSpecCert era fn
  where
  txCertSpec :: CertEnv era -> CertState era -> Specification fn (TxCert era)
  txCertKey :: TxCert era -> CertKey

instance IsConwayUniv fn => EraSpecCert ShelleyEra fn where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
instance IsConwayUniv fn => EraSpecCert AllegraEra fn where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
instance IsConwayUniv fn => EraSpecCert MaryEra fn where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
instance IsConwayUniv fn => EraSpecCert AlonzoEra fn where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
instance IsConwayUniv fn => EraSpecCert BabbageEra fn where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
instance IsConwayUniv fn => EraSpecCert ConwayEra fn where
  txCertSpec = conwayTxCertSpec
  txCertKey = conwayTxCertKey

-- | Used to aggregate the key used in registering a Certificate. Different
--   certificates use different kinds of Keys, that allows us to use one
--   type to represent all kinds of keys (Similar to DepositPurpose)
data CertKey
  = StakeKey !(Credential 'Staking)
  | PoolKey !(KeyHash 'StakePool)
  | DRepKey !(Credential 'DRepRole)
  | ColdKey !(Credential 'ColdCommitteeRole)
  | GenesisKey !(KeyHash 'Genesis)
  | MirKey !MIRPot
  deriving (Eq, Show, Ord)

-- | Compute the aggregate key type of a Certificater
conwayTxCertKey :: ConwayTxCert era -> CertKey
conwayTxCertKey (ConwayTxCertDeleg (ConwayRegCert x _)) = StakeKey x
conwayTxCertKey (ConwayTxCertDeleg (ConwayUnRegCert x _)) = StakeKey x
conwayTxCertKey (ConwayTxCertDeleg (ConwayDelegCert x _)) = StakeKey x
conwayTxCertKey (ConwayTxCertDeleg (ConwayRegDelegCert x _ _)) = StakeKey x
conwayTxCertKey (ConwayTxCertPool (RegPool x)) = PoolKey (ppId x)
conwayTxCertKey (ConwayTxCertPool (RetirePool x _)) = PoolKey x
conwayTxCertKey (ConwayTxCertGov (ConwayRegDRep x _ _)) = DRepKey x
conwayTxCertKey (ConwayTxCertGov (ConwayUnRegDRep x _)) = DRepKey x
conwayTxCertKey (ConwayTxCertGov (ConwayUpdateDRep x _)) = DRepKey x
conwayTxCertKey (ConwayTxCertGov (ConwayAuthCommitteeHotKey x _)) = ColdKey x
conwayTxCertKey (ConwayTxCertGov (ConwayResignCommitteeColdKey x _)) = ColdKey x

shelleyTxCertKey :: ShelleyTxCert era -> CertKey
shelleyTxCertKey (ShelleyTxCertDelegCert (ShelleyRegCert x)) = StakeKey x
shelleyTxCertKey (ShelleyTxCertDelegCert (ShelleyUnRegCert x)) = StakeKey x
shelleyTxCertKey (ShelleyTxCertDelegCert (ShelleyDelegCert x _)) = StakeKey x
shelleyTxCertKey (ShelleyTxCertPool (RegPool x)) = PoolKey (ppId x)
shelleyTxCertKey (ShelleyTxCertPool (RetirePool x _)) = PoolKey x
shelleyTxCertKey (ShelleyTxCertGenesisDeleg (GenesisDelegCert a _ _)) = GenesisKey a
shelleyTxCertKey (ShelleyTxCertMir (MIRCert p _)) = MirKey p

-- =====================================================

testGenesisCert ::
  forall era. (AtMostEra BabbageEra era, EraSpecDeleg era, EraSpecPParams era) => Gen Property
testGenesisCert = do
  dstate <- genFromSpec @ConwayFn @(DState era) dStateSpec
  let spec = genesisDelegCertSpec @ConwayFn dstate
  ans <- genFromSpec @ConwayFn spec
  pure $ property (conformsToSpec ans spec)

testShelleyCert ::
  forall era. (AtMostEra BabbageEra era, EraSpecPParams era, EraSpecDeleg era) => Gen Property
testShelleyCert = do
  env <- genFromSpec @ConwayFn @(CertEnv era) certEnvSpec
  dstate <- genFromSpec @ConwayFn @(CertState era) certStateSpecEx
  let spec = shelleyTxCertSpec env dstate
  ans <- genFromSpec @ConwayFn spec
  let tag = case ans of
        ShelleyTxCertDelegCert x -> case x of
          ShelleyRegCert {} -> "Register"
          ShelleyUnRegCert {} -> "UnRegister"
          ShelleyDelegCert {} -> "Delegate"
        ShelleyTxCertPool x -> case x of
          RegPool {} -> "RegPool"
          RetirePool {} -> "RetirePool"
        ShelleyTxCertGenesisDeleg _ -> "Genesis"
        ShelleyTxCertMir _ -> "Mir"
  pure (classify True tag (property (conformsToSpec ans spec)))

testConwayCert :: Gen Property
testConwayCert = do
  env <- genFromSpec @ConwayFn @(CertEnv ConwayEra) certEnvSpec
  dstate <- genFromSpec @ConwayFn @(CertState ConwayEra) certStateSpecEx
  let spec = conwayTxCertSpec env dstate
  ans <- genFromSpec @ConwayFn spec
  let tag = case ans of
        (ConwayTxCertDeleg (ConwayRegCert _ _)) -> "Register"
        (ConwayTxCertDeleg (ConwayUnRegCert _ _)) -> "UnRegister"
        (ConwayTxCertDeleg (ConwayDelegCert _ _)) -> "Delegate"
        (ConwayTxCertDeleg (ConwayRegDelegCert _ _ _)) -> "Register&Delegate"
        (ConwayTxCertPool (RegPool _)) -> "RegPool"
        (ConwayTxCertPool (RetirePool _ _)) -> "RetirePool"
        (ConwayTxCertGov (ConwayRegDRep _ _ _)) -> "RegDRep"
        (ConwayTxCertGov (ConwayUnRegDRep _ _)) -> "UnRegDRep"
        (ConwayTxCertGov (ConwayUpdateDRep _ _)) -> "UpdateDRep"
        (ConwayTxCertGov (ConwayAuthCommitteeHotKey _ _)) -> "AuthCommittee"
        (ConwayTxCertGov (ConwayResignCommitteeColdKey _ _)) -> "ResignCommittee"
  pure (classify True tag (conformsToSpec ans spec))
