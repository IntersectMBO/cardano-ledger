{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.Conway.Rules (GovEnv (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState
import Constrained.API
import Data.Map (Map)
import Test.Cardano.Ledger.Constrained.Conway.Deleg (witnessedKeyHashStakePoolMapSpec)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (
  accountStateSpec,
  conwayCertStateSpec,
  conwayDStateSpec,
  conwayGovStateSpec,
  epochNoSpec,
  epochStateSpec,
  genCertContext,
  getMarkSnapShot,
  govEnvSpec,
  ledgerStateSpec,
  newEpochStateSpec,
  pStateSpec,
  snapShotSpec,
  snapShotsSpec,
  utxoSpecWit,
  utxoStateSpec,
  vStateSpec,
  whoDelegatesSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.ParametricSpec (EraSpecTxOut (..))
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse (GenScript (..), genWitUniv)
import Test.QuickCheck (Gen)

-- ==============================================================
-- Generators for all the types found in the Ledger State.
-- ==============================================================

ppX :: forall era. EraSpecPParams era => Gen (PParams era)
ppX = genFromSpec @(PParams era) pparamsSpec

acctX :: Gen ChainAccountState
acctX = genFromSpec @ChainAccountState accountStateSpec

psX :: forall era. GenScript era => Gen (PState era)
psX = do
  univ <- genWitUniv @era 25
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(PState era) (pStateSpec univ (lit epoch))

dsX :: forall era. era ~ ConwayEra => Gen (DState era)
dsX = do
  univ <- genWitUniv @era 100
  context <- genCertContext @era univ
  khppMap <- genFromSpec (witnessedKeyHashStakePoolMapSpec univ)
  genFromSpec @(DState era) (conwayDStateSpec univ context (lit khppMap))

conwayDStateGen ::
  forall era. era ~ ConwayEra => Gen (DState era)
conwayDStateGen = dsX @ConwayEra

vsX :: forall era. (GenScript era, era ~ ConwayEra) => Gen (VState era)
vsX = do
  univ <- genWitUniv 25
  epoch <- genFromSpec @EpochNo epochNoSpec
  whodelegates <- genFromSpec (whoDelegatesSpec univ)
  genFromSpec @(VState era) (vStateSpec univ (lit epoch) whodelegates)

csX :: forall era. era ~ ConwayEra => Gen (CertState era)
csX = do
  univ <- genWitUniv @era 300
  context <- genCertContext @era univ
  genFromSpec (conwayCertStateSpec univ context (lit (EpochNo 10)))

utxoX :: forall era. era ~ ConwayEra => Gen (UTxO era)
utxoX = do
  univ <- genWitUniv @era 50
  cs <-
    genFromSpec
      @(Map (Credential Staking) (KeyHash StakePool))
      (hasSize (rangeSize 30 30))
  genFromSpec @(UTxO era) (utxoSpecWit @era univ (lit cs))

utxostateX ::
  forall era.
  (era ~ ConwayEra, HasSpec (InstantStake era)) =>
  PParams era ->
  Gen (UTxOState era)
utxostateX pp = do
  univ <- genWitUniv @era 50
  certstate <- csX @era
  genFromSpec @(UTxOState era) (utxoStateSpec pp univ (lit certstate))

govenvX :: PParams ConwayEra -> Gen (GovEnv ConwayEra)
govenvX pp = genFromSpec @(GovEnv ConwayEra) (govEnvSpec pp)

conwaygovX :: PParams ConwayEra -> Gen (ConwayGovState ConwayEra)
conwaygovX pp = do
  env <- genFromSpec @(GovEnv ConwayEra) (govEnvSpec pp)
  genFromSpec @(ConwayGovState ConwayEra) (conwayGovStateSpec pp env)

lsX ::
  forall era. (era ~ ConwayEra, HasSpec (InstantStake era)) => PParams era -> Gen (LedgerState era)
lsX pp = do
  univ <- genWitUniv @era 200
  context <- genCertContext @era univ
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(LedgerState era) (ledgerStateSpec pp univ context (lit epoch))

esX ::
  forall era.
  (era ~ ConwayEra, HasSpec (InstantStake era)) =>
  PParams era ->
  Gen (EpochState era)
esX pp = do
  univ <- genWitUniv @era 200
  context <- genCertContext @era univ
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(EpochState era) (epochStateSpec pp univ context (lit epoch))

nesX ::
  forall era.
  (era ~ ConwayEra, HasSpec (InstantStake era)) =>
  PParams era ->
  Gen (NewEpochState era)
nesX pp = do
  univ <- genWitUniv @era 200
  context <- genCertContext @era univ
  genFromSpec @(NewEpochState era) (newEpochStateSpec pp univ context)

snapX :: Gen SnapShot
snapX = genFromSpec @SnapShot snapShotSpec

snapsX ::
  forall era.
  ( HasSpec (InstantStake era)
  , era ~ ConwayEra
  ) =>
  PParams era ->
  Gen SnapShots
snapsX pp = do
  univ <- genWitUniv @era 200
  context <- genCertContext @era univ
  epoch <- genFromSpec @EpochNo epochNoSpec
  ls <- genFromSpec @(LedgerState era) (ledgerStateSpec pp univ context (lit epoch))
  genFromSpec @SnapShots (snapShotsSpec (lit (getMarkSnapShot ls)))

instanRewX :: forall era. (era ~ ConwayEra, EraSpecTxOut era) => Gen InstantaneousRewards
instanRewX = do
  univ <- genWitUniv @era 50
  acct <- genFromSpec @ChainAccountState accountStateSpec
  genFromSpec @InstantaneousRewards
    (irewardSpec @era univ (lit acct))

{-
-- ==============================================================
-- The WellFormed class
-- ==============================================================

class
  ( era ~ ConwayEra
  , HasSpec (InstantStake era)
  , HasSpec t
  ) =>
  WellFormed t era
  where
  -- \| Well formed with PParams as input
  wffWithPP :: PParams era -> Gen t
  wffWithPP _ = wff @t @era

  -- \| Generate a constrained PParams if one is needed
  wff :: Gen t
  wff = do
    pp <- ppX @era
    wffWithPP pp

-- ==============================================================
-- The WellFormed instances
-- ==============================================================

instance (EraSpecPParams era, HasSpec (InstantStake era)) => WellFormed (PParams era) era where
  wff = ppX @era
  wffWithPP = pure

instance (EraSpecPParams era, HasSpec (InstantStake era)) => WellFormed ChainAccountState era where
  wff = acctX
  wffWithPP _ = acctX

instance
  (GenScript era, HasSpec (InstantStake era), EraSpecPParams era) =>
  WellFormed (PState era) era
  where
  wff = psX
  wffWithPP _ = psX

instance WellFormed (DState ConwayEra) ConwayEra where
  wff = conwayDStateGen
  wffWithPP _ = conwayDStateGen

instance
  (GenScript era, HasSpec (InstantStake era), EraSpecPParams era) =>
  WellFormed (VState era) era
  where
  wff = vsX
  wffWithPP _ = vsX

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , CertState era ~ ShelleyCertState era
  ) =>
  WellFormed (ShelleyCertState era) era
  where
  wff = csX @era

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , CertState era ~ ConwayCertState era
  ) =>
  WellFormed (ConwayCertState era) era
  where
  wff = csX @era

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , IsNormalType (CertState era)
  ) =>
  WellFormed (UTxO era) era
  where
  wff = utxoX

instance
  ( EraSpecPParams era
  , IsNormalType (CertState era)
  , HasSpec (InstantStake era)
  ) =>
  WellFormed (UTxOState era) era
  where
  wffWithPP = utxostateX

instance WellFormed (GovEnv ConwayEra) ConwayEra where
  wffWithPP = govenvX

instance WellFormed (ConwayGovState ConwayEra) ConwayEra where
  wffWithPP = conwaygovX

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , IsNormalType (CertState era)
  ) =>
  WellFormed (ShelleyGovState era) era
  where
  wffWithPP pp = genFromSpec @(ShelleyGovState era) (shelleyGovStateSpec pp)

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , IsNormalType (CertState era)
  ) =>
  WellFormed (LedgerState era) era
  where
  wffWithPP = lsX

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , IsNormalType (CertState era)
  ) =>
  WellFormed (EpochState era) era
  where
  wffWithPP = esX

instance
  (EraSpecPParams era, HasSpec (InstantStake era)) =>
  WellFormed (NewEpochState era) era
  where
  wffWithPP = nesX

instance (EraSpecPParams era, HasSpec (InstantStake era)) => WellFormed SnapShot era where
  wff = snapX

instance
  ( EraSpecPParams era
  , HasSpec (InstantStake era)
  , IsNormalType (CertState era)
  ) =>
  WellFormed SnapShots era
  where
  wffWithPP = snapsX @era

instance
  (EraSpecPParams era, HasSpec (InstantStake era), EraSpecTxOut era) =>
  WellFormed InstantaneousRewards era
  where
  wff = instanRewX @era
-}
