{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.PropertyTests (
  commonTests,
)
where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (LedgerState)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Control.Monad.Trans.Reader (ReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Functor.Identity (Identity)
import Test.Cardano.Protocol.TPraos.Core (GenEnv)
import Test.Cardano.Protocol.TPraos.EraGen (EraGen)
import Test.Cardano.Protocol.TPraos.Rules (CHAIN)

import qualified Test.Cardano.Ledger.Shelley.ByronTranslation as ByronTranslation (testGroupByronTranslation)
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import qualified Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces as ClassifyTraces (
  onlyValidChainSignalsAreGenerated,
  onlyValidLedgerSignalsAreGenerated,
  relevantCasesAreCovered,
 )
import qualified Test.Cardano.Ledger.Shelley.Rules.CollisionFreeness as ColllisionFree (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.Deleg as Deleg (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.IncrementalStake as IncrementalStake (incrStakeComputationTest)
import qualified Test.Cardano.Ledger.Shelley.Rules.Pool as Pool (tests)
import qualified Test.Cardano.Ledger.Shelley.Rules.PoolReap as PoolReap (tests)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import qualified Test.Cardano.Ledger.Shelley.ShelleyTranslation as ShelleyTranslation (testGroupShelleyTranslation)
import Test.Cardano.Protocol.TPraos.Utils (ChainProperty)
import Test.QuickCheck (Args (maxSuccess), stdArgs)
import Test.Tasty (TestTree, localOption, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

commonTests ::
  forall era ledger.
  ( EraGen era
  , EraGovernance era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , QC.HasTrace ledger (GenEnv era)
  , Embed (EraRule "DELEGS" era) ledger
  , Embed (EraRule "UTXOW" era) ledger
  , Environment ledger ~ LedgerEnv era
  , QC.BaseEnv ledger ~ Globals
  , BaseM ledger ~ ReaderT Globals Identity
  , State ledger ~ LedgerState era
  , Signal ledger ~ Tx era
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  ) =>
  [TestTree]
commonTests =
  [ (localOption (TQC.QuickCheckMaxRatio 100) $ (ClassifyTraces.relevantCasesAreCovered @era (maxSuccess stdArgs)))
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
