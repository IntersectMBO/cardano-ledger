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

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes (Globals, ShelleyBase, SlotNo)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (ApplyBlock, ShelleyPOOL)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (LedgerState, NewEpochState)
import Cardano.Ledger.Shelley.Rules (BbodyEnv, LedgerEnv, ShelleyBbodyState, ShelleyLedgersEnv)
import Cardano.Ledger.Shelley.State
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.Rules.Tickn (TicknEnv, TicknState)
import Control.State.Transition
import Data.Sequence (Seq)
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
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (Args (maxSuccess), stdArgs)
import Test.Tasty (TestTree, localOption, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

commonTests ::
  forall era.
  ( EraGen era
  , EraStake era
  , ShelleyEraAccounts era
  , ApplyBlock era
  , GetLedgerView era
  , Embed (EraRule "BBODY" era) (CHAIN era)
  , Embed (EraRule "TICK" era) (CHAIN era)
  , Embed (EraRule "TICKN" era) (CHAIN era)
  , QC.HasTrace (EraRule "LEDGERS" era) (GenEnv MockCrypto era)
  , State (EraRule "TICKN" era) ~ TicknState
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Environment (EraRule "TICKN" era) ~ TicknEnv
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , Signal (EraRule "TICKN" era) ~ Bool
  , BaseM (EraRule "LEDGERS" era) ~ ShelleyBase
  , AtMostEra "Alonzo" era
  , GovState era ~ ShelleyGovState era
  , InstantStake era ~ ShelleyInstantStake era
  , QC.BaseEnv (EraRule "LEDGER" era) ~ Globals
  , QC.HasTrace (EraRule "LEDGER" era) (GenEnv MockCrypto era)
  , State (EraRule "TICK" era) ~ NewEpochState era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Environment (EraRule "TICK" era) ~ ()
  , Signal (EraRule "LEDGER" era) ~ Tx TopTx era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , EraRule "POOL" era ~ ShelleyPOOL era
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
      [ AdaPreservation.tests @era (maxSuccess stdArgs)
      , ColllisionFree.tests @era
      , IncrementalStake.incrStakeComputationTest @era
      ]
  , testGroup
      "Trace generators properties"
      [ ClassifyTraces.onlyValidLedgerSignalsAreGenerated @era @(EraRule "LEDGER" era)
      , ClassifyTraces.onlyValidChainSignalsAreGenerated @era
      ]
  , ByronTranslation.testGroupByronTranslation
  , ShelleyTranslation.testGroupShelleyTranslation
  ]
