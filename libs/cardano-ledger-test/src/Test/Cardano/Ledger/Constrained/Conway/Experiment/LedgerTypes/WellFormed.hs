{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -O0 #-}
#endif

module Test.Cardano.Ledger.Constrained.Conway.Experiment.LedgerTypes.WellFormed where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules (GovEnv (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.CertState (ShelleyCertState)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State (SnapShot (..), SnapShots (..))
import Constrained.Experiment.API
import Data.Map (Map)

-- import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Instances
import Test.Cardano.Ledger.Constrained.Conway.Experiment.LedgerTypes.Specs (
  EraSpecLedger (..),
  accountStateSpec,
  aggregateDRep,
  conwayGovStateSpec,
  dstateSpec,
  epochNoSpec,
  epochStateSpec,
  getMarkSnapShot,
  govEnvSpec,
  ledgerStateSpec,
  pstateSpec,
  shelleyGovStateSpec,
  snapShotSpec,
  snapShotsSpec,
  utxoSpecWit,
  utxoStateSpec,
  vstateSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.Experiment.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.Experiment.ParametricSpec (EraSpecTxOut (..))
import Test.Cardano.Ledger.Constrained.Conway.Experiment.WitnessUniverse (
  GenScript (..),
  genWitUniv,
 )
import Test.QuickCheck (Gen)

-- ==============================================================
-- Generators for all the types found in the Ledger State.
-- ==============================================================

ppX :: forall era. EraSpecPParams era => Gen (PParams era)
ppX = genFromSpec @(PParams era) pparamsSpec

acctX :: Gen AccountState
acctX = genFromSpec @AccountState accountStateSpec

psX :: forall era. GenScript era => Gen (PState era)
psX = do
  univ <- genWitUniv 25
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(PState era) (pstateSpec univ (lit epoch))

dsX :: forall era. EraSpecLedger era => Gen (DState era)
dsX = do
  univ <- genWitUniv 25
  acct <- genFromSpec @AccountState accountStateSpec
  pools <-
    genFromSpec @(Map (KeyHash 'StakePool) PoolParams)
      (hasSize (rangeSize 8 8))
  genFromSpec @(DState era) (dstateSpec @era univ (lit acct) (lit pools))

vsX :: forall era. GenScript era => Gen (VState era)
vsX = do
  univ <- genWitUniv 25
  epoch <- genFromSpec @EpochNo epochNoSpec
  delegatees <-
    aggregateDRep
      <$> genFromSpec -- ensures that each credential delegates to exactly one DRep
        @(Map (Credential 'Staking) DRep)
        TrueSpec
  genFromSpec @(VState era) (vstateSpec univ (lit epoch) (lit delegatees))

csX :: forall era. EraSpecLedger era => Gen (CertState era)
csX = do
  univ <- genWitUniv 25
  acct <- genFromSpec @AccountState accountStateSpec
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(CertState era)
    (certStateSpec univ (lit acct) (lit epoch))

utxoX :: forall era. EraSpecLedger era => Gen (UTxO era)
utxoX = do
  univ <- genWitUniv @era 50
  cs <-
    genFromSpec
      @(Map (Credential 'Staking) (KeyHash 'StakePool))
      (hasSize (rangeSize 30 30))
  genFromSpec @(UTxO era) (utxoSpecWit @era univ (lit cs))

utxostateX ::
  forall era.
  ( EraSpecLedger era
  , -- TODO: this is temporary, remove once `utxoStateSpec` is general enough
    CertState era ~ ShelleyCertState era
  ) =>
  PParams era -> Gen (UTxOState era)
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
  forall era.
  ( EraSpecLedger era
  , -- TODO: remove once `ledgerStateSpec` is general enough
    CertState era ~ ShelleyCertState era
  ) =>
  PParams era -> Gen (LedgerState era)
lsX pp = do
  univ <- genWitUniv @era 50
  acct <- genFromSpec @AccountState accountStateSpec
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(LedgerState era) (ledgerStateSpec pp univ (lit acct) (lit epoch))

esX ::
  forall era.
  (EraSpecLedger era, CertState era ~ ShelleyCertState era) =>
  PParams era -> Gen (EpochState era)
esX pp = do
  univ <- genWitUniv @era 50
  epoch <- genFromSpec @EpochNo epochNoSpec
  genFromSpec @(EpochState era) (epochStateSpec pp univ (lit epoch))

nesX :: forall era. EraSpecLedger era => PParams era -> Gen (NewEpochState era)
nesX pp = do
  univ <- genWitUniv @era 50
  genFromSpec @(NewEpochState era) (newEpochStateSpec pp univ)

snapX :: Gen SnapShot
snapX = genFromSpec @SnapShot snapShotSpec

snapsX ::
  forall era.
  (EraSpecLedger era, CertState era ~ ShelleyCertState era) => PParams era -> Gen SnapShots
snapsX pp = do
  univ <- genWitUniv @era 50
  acct <- genFromSpec @AccountState accountStateSpec
  epoch <- genFromSpec @EpochNo epochNoSpec
  ls <- genFromSpec @(LedgerState era) (ledgerStateSpec pp univ (lit acct) (lit epoch))
  genFromSpec @SnapShots (snapShotsSpec (lit (getMarkSnapShot ls)))

instanRewX :: forall era. EraSpecTxOut era => Gen InstantaneousRewards
instanRewX = do
  univ <- genWitUniv @era 50
  acct <- genFromSpec @AccountState accountStateSpec
  genFromSpec @InstantaneousRewards
    (irewardSpec @era univ (lit acct))

-- ==============================================================
-- The WellFormed class
-- ==============================================================

class (EraSpecPParams era, HasSpec t) => WellFormed t era where
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

instance EraSpecPParams era => WellFormed (PParams era) era where
  wff = ppX @era
  wffWithPP p = pure p

instance EraSpecPParams era => WellFormed AccountState era where
  wff = acctX
  wffWithPP _ = acctX

instance (GenScript era, EraSpecPParams era) => WellFormed (PState era) era where
  wff = psX
  wffWithPP _ = psX

instance (EraSpecPParams era, EraSpecLedger era) => WellFormed (DState era) era where
  wff = dsX
  wffWithPP _ = dsX

instance (GenScript era, EraSpecPParams era) => WellFormed (VState era) era where
  wff = vsX
  wffWithPP _ = vsX

instance
  (EraSpecPParams era, EraSpecLedger era, CertState era ~ ShelleyCertState era) =>
  WellFormed (ShelleyCertState era) era
  where
  wff = csX

instance (EraSpecPParams era, EraSpecLedger era) => WellFormed (UTxO era) era where
  wff = utxoX

instance
  (EraSpecPParams era, EraSpecLedger era, CertState era ~ ShelleyCertState era) =>
  WellFormed (UTxOState era) era
  where
  wffWithPP = utxostateX

instance WellFormed (GovEnv ConwayEra) ConwayEra where
  wffWithPP pp = govenvX pp

instance WellFormed (ConwayGovState ConwayEra) ConwayEra where
  wffWithPP pp = conwaygovX pp

instance (EraSpecPParams era, EraSpecLedger era) => WellFormed (ShelleyGovState era) era where
  wffWithPP pp = genFromSpec @(ShelleyGovState era) (shelleyGovStateSpec pp)

instance
  (EraSpecPParams era, EraSpecLedger era, CertState era ~ ShelleyCertState era) =>
  WellFormed (LedgerState era) era
  where
  wffWithPP = lsX

instance
  (EraSpecPParams era, EraSpecLedger era, CertState era ~ ShelleyCertState era) =>
  WellFormed (EpochState era) era
  where
  wffWithPP = esX

instance (EraSpecPParams era, EraSpecLedger era) => WellFormed (NewEpochState era) era where
  wffWithPP = nesX

instance EraSpecPParams era => WellFormed SnapShot era where
  wff = snapX

instance
  (EraSpecPParams era, EraSpecLedger era, CertState era ~ ShelleyCertState era) =>
  WellFormed SnapShots era
  where
  wffWithPP = snapsX @era

instance
  (EraSpecPParams era, EraSpecTxOut era) =>
  WellFormed InstantaneousRewards era
  where
  wff = instanRewX @era
