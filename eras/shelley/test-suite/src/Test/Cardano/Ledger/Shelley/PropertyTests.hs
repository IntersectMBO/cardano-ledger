{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.PropertyTests (
  commonTests,
) where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (LedgerState)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.Shelley.State
import Control.Monad.Trans.Reader (ReaderT)
import Control.State.Transition
import Data.Functor.Identity (Identity)
import qualified Test.Cardano.Ledger.Shelley.ByronTranslation as ByronTranslation (
  testGroupByronTranslation,
 )
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import qualified Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces as ClassifyTraces (
  onlyValidChainSignalsAreGenerated,
  onlyValidLedgerSignalsAreGenerated,
  relevantCasesAreCovered,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.CollisionFreeness as ColllisionFree (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.Deleg as Deleg (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.IncrementalStake as IncrementalStake (
  incrStakeComputationTest,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.Pool as Pool (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.PoolReap as PoolReap (tests)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import qualified Test.Cardano.Ledger.Shelley.ShelleyTranslation as ShelleyTranslation (
  testGroupShelleyTranslation,
 )
import Test.Cardano.Ledger.Shelley.Utils (ChainProperty)
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (Args (maxSuccess), stdArgs)
import Test.Tasty (TestTree, localOption, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

commonTests ::
  forall era ledger.
  ( EraGen era
  , EraStake era
  , InstantStake era ~ ShelleyInstantStake era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  , QC.HasTrace ledger (GenEnv MockCrypto era)
  , Embed (EraRule "DELEGS" era) ledger
  , Embed (EraRule "UTXOW" era) ledger
  , Environment ledger ~ LedgerEnv era
  , QC.BaseEnv ledger ~ Globals
  , BaseM ledger ~ ReaderT Globals Identity
  , State ledger ~ LedgerState era
  , Signal ledger ~ Tx era
  , GovState era ~ ShelleyGovState era
  ) =>
  [TestTree]
commonTests =
  [ localOption (TQC.QuickCheckMaxRatio 100) $
      ClassifyTraces.relevantCasesAreCovered @era (maxSuccess stdArgs)
  , Deleg.tests @era
  , Pool.tests @era
  , PoolReap.tests @era
  , testGroup
      "CHAIN level Properties"
      [ AdaPreservation.tests @era @ledger (maxSuccess stdArgs)
      , ColllisionFree.tests @era @ledger
      , IncrementalStake.incrStakeComputationTest @era @ledger
      ]
  , testGroup
      "Trace generators properties"
      [ ClassifyTraces.onlyValidLedgerSignalsAreGenerated @era @ledger
      , ClassifyTraces.onlyValidChainSignalsAreGenerated @era
      ]
  , ByronTranslation.testGroupByronTranslation
  , ShelleyTranslation.testGroupShelleyTranslation
  ]
