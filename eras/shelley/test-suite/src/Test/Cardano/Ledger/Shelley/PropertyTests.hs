{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.PropertyTests (
  propertyTests,
  minimalPropertyTests,
  relevantCasesAreCovered,
  delegProps,
  poolProps,
  poolReapProps,
  adaPreservationProps,
  collisionFreeProps,
  onlyValidLedgerSignalsAreGenerated,
  onlyValidChainSignalsAreGenerated,
  -- Crypto era only
  depositsProps,
)
where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.API (LedgerState)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
import Control.Monad.Trans.Reader (ReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Functor.Identity (Identity)
import Data.List (nub, sort)
import Data.Set as Set (fromList, singleton)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Shelley.Address.Bootstrap (
  bootstrapHashTest,
 )
import Test.Cardano.Ledger.Shelley.ByronTranslation (testGroupByronTranslation)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Rules.AdaPreservation (adaPreservationProps)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces (
  onlyValidChainSignalsAreGenerated,
  onlyValidLedgerSignalsAreGenerated,
  relevantCasesAreCovered,
 )

import Test.Cardano.Ledger.Shelley.Rules.CollisionFreeness (collisionFreeProps)
import Test.Cardano.Ledger.Shelley.Rules.Deleg (delegProps)
import Test.Cardano.Ledger.Shelley.Rules.Deposits (depositsProps)
import Test.Cardano.Ledger.Shelley.Rules.IncrementalStake (incrStakeComputationProp)
import Test.Cardano.Ledger.Shelley.Rules.Pool (poolProps)
import Test.Cardano.Ledger.Shelley.Rules.PoolReap (poolReapProps)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (TestingLedger)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.ShelleyTranslation (testGroupShelleyTranslation)
import Test.Cardano.Ledger.Shelley.Utils (ChainProperty, RawSeed, mkKeyPair')
import Test.QuickCheck (conjoin, (===), (==>))
import Test.Tasty (TestTree, localOption, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

-- =====================================================================

propWitVKeys ::
  forall c.
  (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  RawSeed ->
  SafeHash c EraIndependentTxBody ->
  SafeHash c EraIndependentTxBody ->
  TQC.Property
propWitVKeys seed h1 h2 =
  let kp = mkKeyPair' seed
      w1 = mkWitnessVKey h1 kp
      w2 = mkWitnessVKey h2 kp
   in conjoin
        [ sort [w1, w2] === sort [w2, w1]
        , length (nub [w1, w2]) === length (Set.fromList [w1, w2])
        , w1 /= w2 ==> length (Set.singleton w1 <> Set.singleton w2) === 2
        ]

minimalPropertyTests ::
  forall era ledger.
  ( EraGen era
  , EraGovernance era
  , Tx era ~ ShelleyTx era
  , TestingLedger era ledger
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv era)
  , ProtVerAtMost era 8
  , GovernanceState era ~ ShelleyPPUPState era
  ) =>
  TestTree
minimalPropertyTests =
  testGroup
    "Minimal Property Tests"
    [ (localOption (TQC.QuickCheckMaxRatio 50) $ TQC.testProperty "Chain and Ledger traces cover the relevant cases" (relevantCasesAreCovered @era))
    , TQC.testProperty "total amount of Ada is preserved (Chain)" (adaPreservationProps @era @ledger)
    , TQC.testProperty "Only valid CHAIN STS signals are generated" (onlyValidChainSignalsAreGenerated @era)
    , bootstrapHashTest
    , TQC.testProperty "WitVKey does not brake containers due to invalid Ord" $
        propWitVKeys @(EraCrypto era)
    ]

-- | 'TestTree' of property-based testing properties.
propertyTests ::
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
  TestTree
propertyTests =
  testGroup
    "Property-Based Testing"
    [ testGroup
        "Classify Traces"
        [ ( localOption (TQC.QuickCheckMaxRatio 100) $
              TQC.testProperty
                "Chain and Ledger traces cover the relevant cases"
                (relevantCasesAreCovered @era)
          )
        ]
    , testGroup
        "STS Rules - Delegation Properties"
        [ TQC.testProperty
            "properties of the DELEG STS"
            (delegProps @era)
        ]
    , testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "properties of the POOL STS"
            (poolProps @era)
        ]
    , testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            (poolReapProps @era)
        ]
    , testGroup
        "CHAIN level Properties"
        [ TQC.testProperty
            "collection of Ada preservation properties"
            (adaPreservationProps @era @ledger)
        , TQC.testProperty
            "inputs are eliminated, outputs added to utxo and TxIds are unique"
            (collisionFreeProps @era @ledger)
        , TQC.testProperty
            "incremental stake calc"
            (incrStakeComputationProp @era @ledger)
        ]
    , testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            (onlyValidLedgerSignalsAreGenerated @era @ledger)
        , TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            (onlyValidChainSignalsAreGenerated @era)
        ]
    , testGroupByronTranslation
    , testGroupShelleyTranslation
    ]
