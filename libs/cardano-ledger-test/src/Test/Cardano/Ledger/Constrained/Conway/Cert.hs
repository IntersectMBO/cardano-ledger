{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Ledger.Shelley.TxCert (ShelleyTxCert (..))
import Constrained.API
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Test.Cardano.Ledger.Constrained.Conway.Pool
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.QuickCheck hiding (forAll, witness)

certEnvSpec ::
  forall era.
  EraSpecPParams era =>
  WitUniv era -> Specification (CertEnv era)
certEnvSpec _univ =
  constrained $ \ce ->
    match ce $ \pp _currEpoch _currCommittee _proposals ->
      [ satisfies pp pparamsSpec
      ]

delegateeSpec ::
  Era era =>
  WitUniv era ->
  Specification (Set (Credential 'DRepRole))
delegateeSpec univ = constrained $ \x ->
  [ witness univ x
  , assert $ sizeOf_ x <=. 20
  , assert $ sizeOf_ x >=. 10
  ]

shelleyCertStateSpec ::
  forall era.
  (EraSpecDeleg era, EraCertState era) =>
  WitUniv era ->
  Set (Credential 'DRepRole) ->
  Map (RewardAccount) Coin ->
  Specification (ShelleyCertState era)
shelleyCertStateSpec univ _delegatees wdrls =
  constrained $ \cs ->
    match cs $ \pState dState ->
      [ satisfies pState (pStateSpec @era univ)
      , satisfies dState (dStateSpec @era univ wdrls)
      ]

conwayCertStateSpec ::
  forall era.
  (EraSpecDeleg era, EraCertState era, ConwayEraCertState era) =>
  WitUniv era ->
  Set (Credential 'DRepRole) ->
  Map (RewardAccount) Coin ->
  Specification (ConwayCertState era)
conwayCertStateSpec univ delegatees wdrls =
  constrained $ \cs ->
    match cs $ \vState pState dState ->
      [ satisfies pState (pStateSpec @era univ)
      , satisfies dState (dStateSpec @era univ wdrls)
      , satisfies vState (vStateSpec univ delegatees)
      ]

conwayTxCertSpec ::
  forall era.
  era ~ ConwayEra =>
  WitUniv era ->
  CertEnv era ->
  CertState era ->
  Specification (ConwayTxCert era)
conwayTxCertSpec univ (CertEnv pp ce cc cp) certState =
  constrained $ \txCert ->
    caseOn
      txCert
      -- These weights try to make it equally likely that each of the many certs
      -- across the 3 categories are chosen at similar frequencies.
      (branchW 3 $ \delegCert -> satisfies delegCert $ conwayDelegCertSpec delegEnv certState)
      (branchW 1 $ \poolCert -> satisfies poolCert $ poolCertSpec univ poolEnv certPState)
      (branchW 2 $ \govCert -> satisfies govCert $ govCertSpec univ govCertEnv certState)
  where
    certPState = certState ^. certPStateL
    delegEnv = ConwayDelegEnv pp (psStakePoolParams certPState)
    poolEnv = PoolEnv ce pp
    govCertEnv = ConwayGovCertEnv pp ce cc cp

-- ==============================================================
-- Shelley Certs

-- | Genesis delegations only work through the Babbage era. Hence the (AtMostEra BabbageEra era)
genesisDelegCertSpec ::
  forall era.
  (AtMostEra BabbageEra era, Era era) =>
  DState era -> Specification GenesisDelegCert
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
  forall era.
  (AtMostEra BabbageEra era, EraSpecPParams era) =>
  WitUniv era ->
  CertEnv era ->
  ShelleyCertState era ->
  Specification (ShelleyTxCert era)
shelleyTxCertSpec univ (CertEnv pp currEpoch _ _) (ShelleyCertState pstate dstate) =
  constrained $ \ [var|shelleyTxCert|] ->
    -- These weights try to make it equally likely that each of the many certs
    -- across the 3 categories are chosen at similar frequencies.
    (caseOn shelleyTxCert)
      ( branchW 5 $ \ [var|deleg|] ->
          satisfies
            deleg
            ( shelleyDelegCertSpec @era
                univ
                (ConwayDelegEnv pp (psStakePoolParams pstate))
                dstate
            )
      )
      ( branchW 3 $ \ [var|poolCert|] -> satisfies poolCert $ poolCertSpec univ (PoolEnv currEpoch pp) pstate
      )
      (branchW 1 $ \ [var|genesis|] -> satisfies genesis (genesisDelegCertSpec @era dstate))
      (branchW 1 $ \ [var|_mir|] -> False) -- By design, we never generate a MIR cert

-- =========================================================================
-- Making Cert Era parametric with the EraSpecCert class

class
  ( HasSpec (TxCert era)
  , EraCertState era
  ) =>
  EraSpecCert era
  where
  txCertSpec :: WitUniv era -> CertEnv era -> CertState era -> Specification (TxCert era)
  txCertKey :: TxCert era -> CertKey
  certStateSpec ::
    WitUniv era ->
    Set (Credential 'DRepRole) ->
    Map RewardAccount Coin ->
    Specification (CertState era)

instance EraSpecCert ShelleyEra where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
  certStateSpec = shelleyCertStateSpec

instance EraSpecCert AllegraEra where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
  certStateSpec = shelleyCertStateSpec

instance EraSpecCert MaryEra where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
  certStateSpec = shelleyCertStateSpec

instance EraSpecCert AlonzoEra where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
  certStateSpec = shelleyCertStateSpec

instance EraSpecCert BabbageEra where
  txCertSpec = shelleyTxCertSpec
  txCertKey = shelleyTxCertKey
  certStateSpec = shelleyCertStateSpec

instance EraSpecCert ConwayEra where
  txCertSpec = conwayTxCertSpec
  txCertKey = conwayTxCertKey
  certStateSpec = conwayCertStateSpec

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
  forall era.
  (AtMostEra BabbageEra era, EraSpecDeleg era, EraSpecPParams era, GenScript era) => Gen Property
testGenesisCert = do
  univ <- genWitUniv @era 200
  wdrls <- genFromSpec (constrained $ \x -> witness univ x)
  dstate <- genFromSpec @(DState era) (dStateSpec @era univ wdrls)
  let spec = genesisDelegCertSpec dstate
  ans <- genFromSpec spec
  pure $ property (conformsToSpec ans spec)

testShelleyCert ::
  forall era.
  ( Era era
  , AtMostEra BabbageEra era
  , EraSpecPParams era
  , EraSpecDeleg era
  , GenScript era
  , EraCertState era
  ) =>
  Gen Property
testShelleyCert = do
  univ <- genWitUniv @era 200
  wdrls <- genFromSpec (constrained $ \x -> witness univ x)
  delegatees <- genFromSpec (delegateeSpec univ)
  env <- genFromSpec @(CertEnv era) (certEnvSpec @era univ)
  dstate <-
    genFromSpec @(ShelleyCertState era)
      (shelleyCertStateSpec @era univ delegatees wdrls)
  let spec = shelleyTxCertSpec univ env dstate
  ans <- genFromSpec spec
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
  univ <- genWitUniv @ConwayEra 200
  env <- genFromSpec @(CertEnv ConwayEra) (certEnvSpec @ConwayEra univ)
  wdrls <- genFromSpec (constrained $ \x -> witness univ x)
  delegatees <- genFromSpec (delegateeSpec univ)
  dstate <-
    genFromSpec @(CertState ConwayEra)
      (conwayCertStateSpec univ delegatees wdrls)
  let spec :: Specification (ConwayTxCert ConwayEra)
      spec = conwayTxCertSpec univ env dstate
  ans <- genFromSpec spec
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
