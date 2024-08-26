{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash ()
import Cardano.Ledger.Shelley.LedgerState (
  AccountState (..),
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Constrained.Base (hasSize, rangeSize)
import Data.Map (Map)
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (prettyA))
import Test.QuickCheck (Gen, generate)

-- ==============================================================
-- The WellFormed class and instances
-- ==============================================================

class (HasSpec ConwayFn t, LedgerEra era ConwayFn) => WellFormed t era where
  -- \| Well formed with PParams as input
  wffWithPP :: PParams era -> Gen t
  wffWithPP _ = wff @t @era

  -- \| Generate a constrained PParams if one is needed
  wff :: Gen t
  wff = do
    pp <- genFromSpec @ConwayFn @(PParams era) pparamsSpec
    wffWithPP pp

instance LedgerEra era ConwayFn => WellFormed (PParams era) era where
  wff = genFromSpec @ConwayFn @(PParams era) pparamsSpec
  wffWithPP p = pure p

instance LedgerEra era ConwayFn => WellFormed AccountState era where
  wff = genFromSpec @ConwayFn @AccountState accountStateSpec

instance LedgerEra era ConwayFn => WellFormed (PState era) era where
  wff = do
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    genFromSpec @ConwayFn @(PState era) (pstateSpec (lit epoch))

instance LedgerEra era ConwayFn => WellFormed (DState era) era where
  wff = do
    acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
    pools <-
      genFromSpec @ConwayFn @(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
        (hasSize (rangeSize 8 8))
    genFromSpec @ConwayFn @(DState era) (dstateSpec (lit acct) (lit pools))

instance LedgerEra era ConwayFn => WellFormed (VState era) era where
  wff = do
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    genFromSpec @ConwayFn @(VState era) (vstateSpec (lit epoch))

instance (c ~ EraCrypto era, LedgerEra era ConwayFn) => WellFormed (DRepState c) era where
  wff = do
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    genFromSpec @ConwayFn @(DRepState c) (drepStateSpec (lit epoch))

instance LedgerEra era ConwayFn => WellFormed (CertState era) era where
  wff = do
    acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    genFromSpec @ConwayFn @(CertState era) (certStateSpec (lit acct) (lit epoch))

instance LedgerEra era ConwayFn => WellFormed (UTxO era) era where
  wff = do
    cs <-
      genFromSpec
        @ConwayFn
        @(Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto))
        (hasSize (rangeSize 30 30))
    genFromSpec @ConwayFn @(UTxO era) (utxoSpec @era (lit cs))

instance LedgerEra era ConwayFn => WellFormed (UTxOState era) era where
  wffWithPP pp = do
    certstate <- wff @(CertState era) @era
    genFromSpec @ConwayFn @(UTxOState era) (utxoStateSpec pp (lit certstate))

instance WellFormed (GovEnv Conway) Conway where
  wffWithPP pp = genFromSpec @ConwayFn @(GovEnv Conway) (govEnvSpec pp)

instance WellFormed (ConwayGovState Conway) Conway where
  wffWithPP pp = do
    env <- genFromSpec @ConwayFn @(GovEnv Conway) (govEnvSpec pp)
    genFromSpec @ConwayFn @(ConwayGovState Conway) (conwayGovStateSpec pp env)

instance LedgerEra era ConwayFn => WellFormed (ShelleyGovState era) era where
  wffWithPP pp = genFromSpec @ConwayFn @(ShelleyGovState era) (shelleyGovStateSpec pp)

instance LedgerEra era ConwayFn => WellFormed (LedgerState era) era where
  wffWithPP pp = do
    acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    genFromSpec @ConwayFn @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))

instance (LedgerEra era ConwayFn, c ~ EraCrypto era) => WellFormed (SnapShot c) era where
  wffWithPP _ = genFromSpec @ConwayFn @(SnapShot (EraCrypto era)) snapShotSpec
  wff = genFromSpec @ConwayFn @(SnapShot (EraCrypto era)) snapShotSpec

instance (LedgerEra era ConwayFn, c ~ EraCrypto era) => WellFormed (SnapShots c) era where
  wffWithPP pp = do
    acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    ls <- genFromSpec @ConwayFn @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))
    genFromSpec @ConwayFn @(SnapShots (EraCrypto era)) (snapShotsSpec (lit (getMarkSnapShot ls)))

instance LedgerEra era ConwayFn => WellFormed (EpochState era) era where
  wffWithPP pp = do
    epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
    genFromSpec @ConwayFn @(EpochState era) (epochStateSpec pp (lit epoch))

instance LedgerEra era ConwayFn => WellFormed (NewEpochState era) era where
  wffWithPP pp = genFromSpec @ConwayFn @(NewEpochState era) (newEpochStateSpec pp)

instance (LedgerEra era ConwayFn, c ~ EraCrypto era) => WellFormed (InstantaneousRewards c) era where
  wff = do
    acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
    genFromSpec @ConwayFn @(InstantaneousRewards (EraCrypto era)) (instantaneousRewardsSpec (lit acct))

-- ================================================================

wffIO :: forall t era. (WellFormed t era, PrettyA t) => IO ()
wffIO = do
  t <- generate $ wff @t @era
  putStrLn (show (prettyA t))

wffFam :: forall f era. (WellFormed (f era) era, PrettyA (f era)) => IO ()
wffFam = do
  t <- generate $ wff @(f era) @era
  putStrLn (show (prettyA t))

wffC :: forall f era. (WellFormed (f (EraCrypto era)) era, PrettyA (f (EraCrypto era))) => IO ()
wffC = do
  t <- generate $ wff @(f (EraCrypto era)) @era
  putStrLn (show (prettyA t))
