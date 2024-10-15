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

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules (GovEnv (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash ()
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Constrained.Base (hasSize, rangeSize)
import Data.Map (Map)
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (
  EraSpecLedger (..),
  accountStateSpec,
  aggregateDRep,
  certStateSpec,
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
  utxoSpec,
  utxoStateSpec,
  vstateSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.QuickCheck (Gen)

-- ==============================================================
-- Generators for all the types found in the Ledger State.
-- ==============================================================

ppX :: forall era. EraSpecPParams era => Gen (PParams era)
ppX = genFromSpec @ConwayFn @(PParams era) pparamsSpec

acctX :: Gen AccountState
acctX = genFromSpec @ConwayFn @AccountState accountStateSpec

psX :: forall era. EraSpecPParams era => Gen (PState era)
psX = do
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(PState era) (pstateSpec (lit epoch))

dsX :: forall era. EraSpecLedger era ConwayFn => Gen (DState era)
dsX = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  pools <-
    genFromSpec @ConwayFn @(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
      (hasSize (rangeSize 8 8))
  genFromSpec @ConwayFn @(DState era) (dstateSpec (lit acct) (lit pools))

vsX :: forall era. EraSpecPParams era => Gen (VState era)
vsX = do
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  delegatees <-
    aggregateDRep
      <$> genFromSpec @ConwayFn -- ensures that each credential delegates to exactly one DRep
        @(Map (Credential 'Staking (EraCrypto era)) (DRep (EraCrypto era)))
        TrueSpec
  genFromSpec @ConwayFn @(VState era) (vstateSpec (lit epoch) (lit delegatees))

csX :: forall era. EraSpecLedger era ConwayFn => Gen (CertState era)
csX = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(CertState era) (certStateSpec (lit acct) (lit epoch))

utxoX :: forall era. EraSpecLedger era ConwayFn => Gen (UTxO era)
utxoX = do
  cs <-
    genFromSpec
      @ConwayFn
      @(Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto))
      (hasSize (rangeSize 30 30))
  genFromSpec @ConwayFn @(UTxO era) (utxoSpec @era (lit cs))

utxostateX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (UTxOState era)
utxostateX pp = do
  certstate <- csX @era
  genFromSpec @ConwayFn @(UTxOState era) (utxoStateSpec pp (lit certstate))

govenvX :: PParams Conway -> Gen (GovEnv Conway)
govenvX pp = genFromSpec @ConwayFn @(GovEnv Conway) (govEnvSpec pp)

conwaygovX :: PParams Conway -> Gen (ConwayGovState Conway)
conwaygovX pp = do
  env <- genFromSpec @ConwayFn @(GovEnv Conway) (govEnvSpec pp)
  genFromSpec @ConwayFn @(ConwayGovState Conway) (conwayGovStateSpec pp env)

lsX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (LedgerState era)
lsX pp = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))

esX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (EpochState era)
esX pp = do
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(EpochState era) (epochStateSpec pp (lit epoch))

nesX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (NewEpochState era)
nesX pp = genFromSpec @ConwayFn @(NewEpochState era) (newEpochStateSpec pp)

snapX :: forall era. Era era => Gen (SnapShot (EraCrypto era))
snapX = genFromSpec @ConwayFn @(SnapShot (EraCrypto era)) snapShotSpec

snapsX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (SnapShots (EraCrypto era))
snapsX pp = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  ls <- genFromSpec @ConwayFn @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))
  genFromSpec @ConwayFn @(SnapShots (EraCrypto era)) (snapShotsSpec (lit (getMarkSnapShot ls)))

instanRewX :: forall era. EraSpecTxOut era ConwayFn => Gen (InstantaneousRewards (EraCrypto era))
instanRewX = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  genFromSpec @ConwayFn @(InstantaneousRewards (EraCrypto era))
    (irewardSpec @era @ConwayFn (lit acct))

-- ==============================================================
-- The WellFormed class
-- ==============================================================

class (EraSpecPParams era, HasSpec ConwayFn t) => WellFormed t era where
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

instance EraSpecPParams era => WellFormed (PState era) era where
  wff = psX
  wffWithPP _ = psX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (DState era) era where
  wff = dsX
  wffWithPP _ = dsX

instance EraSpecPParams era => WellFormed (VState era) era where
  wff = vsX
  wffWithPP _ = vsX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (CertState era) era where
  wff = csX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (UTxO era) era where
  wff = utxoX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (UTxOState era) era where
  wffWithPP = utxostateX

instance WellFormed (GovEnv Conway) Conway where
  wffWithPP pp = govenvX pp

instance WellFormed (ConwayGovState Conway) Conway where
  wffWithPP pp = conwaygovX pp

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (ShelleyGovState era) era where
  wffWithPP pp = genFromSpec @ConwayFn @(ShelleyGovState era) (shelleyGovStateSpec pp)

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (LedgerState era) era where
  wffWithPP = lsX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (EpochState era) era where
  wffWithPP = esX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (NewEpochState era) era where
  wffWithPP = nesX

instance (EraSpecPParams era, c ~ EraCrypto era) => WellFormed (SnapShot c) era where
  wff = snapX @era

instance (EraSpecPParams era, EraSpecLedger era ConwayFn, c ~ EraCrypto era) => WellFormed (SnapShots c) era where
  wffWithPP = snapsX @era

instance
  (EraSpecPParams era, c ~ EraCrypto era, EraSpecTxOut era ConwayFn) =>
  WellFormed (InstantaneousRewards c) era
  where
  wff = instanRewX @era
