{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.PropertyTests
  ( propertyTests,
    minimalPropertyTests,
    relevantCasesAreCovered,
    delegProperties,
    poolProperties,
    removedAfterPoolreap,
    adaPreservationChain,
    collisionFreeComplete,
    onlyValidLedgerSignalsAreGenerated,
    onlyValidChainSignalsAreGenerated,
    -- Crypto era only
    propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
  )
where

import Cardano.Ledger.BaseTypes (Globals, StrictMaybe (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable, Hash, KeyRole (Witness))
import Cardano.Ledger.SafeHash (SafeHash)
import Control.Monad.Trans.Reader (ReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Functor.Identity (Identity)
import Data.List (nub, sort)
import Data.Map (Map)
import Data.Sequence.Strict (StrictSeq)
import Data.Set as Set (Set, fromList, singleton)
import GHC.Records (HasField (..))
import Shelley.Spec.Ledger.API (CHAIN, DPState, PPUPState, UTxOState)
import Shelley.Spec.Ledger.Delegation.Certificates (DCert)
import Shelley.Spec.Ledger.PParams (Update (..))
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv)
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.TxBody (TxIn, Wdrl, WitVKey)
import Shelley.Spec.Ledger.UTxO (makeWitnessVKey)
import Test.QuickCheck (conjoin, (===), (==>))
import Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( bootstrapHashTest,
  )
import Test.Shelley.Spec.Ledger.Address.CompactAddr
  ( propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
    propIsBootstrapRedeemer,
  )
import Test.Shelley.Spec.Ledger.ByronTranslation (testGroupByronTranslation)
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen)
import Test.Shelley.Spec.Ledger.Rules.ClassifyTraces
  ( onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    relevantCasesAreCovered,
  )
import Test.Shelley.Spec.Ledger.Rules.TestChain
  ( TestingLedger,
    adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    poolProperties,
    removedAfterPoolreap,
  )
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()
import Test.Shelley.Spec.Ledger.ShelleyTranslation (testGroupShelleyTranslation)
import Test.Shelley.Spec.Ledger.Utils (ChainProperty, RawSeed, mkKeyPair')
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
      w1 = makeWitnessVKey h1 kp
      w2 = makeWitnessVKey h2 kp
   in conjoin
        [ sort [w1, w2] === sort [w2, w1],
          length (nub [w1, w2]) === length (Set.fromList [w1, w2]),
          w1 /= w2 ==> length (Set.singleton w1 <> Set.singleton w2) === 2
        ]

minimalPropertyTests ::
  forall era ledger.
  ( EraGen era,
    TestingLedger era ledger,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era))
  ) =>
  TestTree
minimalPropertyTests =
  testGroup
    "Minimal Property Tests"
    [ (localOption (TQC.QuickCheckMaxRatio 50) $ TQC.testProperty "Chain and Ledger traces cover the relevant cases" (relevantCasesAreCovered @era)),
      TQC.testProperty "total amount of Ada is preserved (Chain)" (adaPreservationChain @era @ledger),
      TQC.testProperty "Only valid CHAIN STS signals are generated" (onlyValidChainSignalsAreGenerated @era),
      bootstrapHashTest,
      testGroup
        "Compact Address Tests"
        [ TQC.testProperty "Compact address round trip" (propCompactAddrRoundTrip @(Crypto era)),
          TQC.testProperty "Compact address binary representation" (propCompactSerializationAgree @(Crypto era)),
          TQC.testProperty "determining address type doesn't force contents" (propDecompactAddrLazy @(Crypto era)),
          TQC.testProperty "reading the keyhash doesn't force the stake reference" (propDecompactShelleyLazyAddr @(Crypto era)),
          TQC.testProperty "isBootstrapRedeemer is equivalent for CompactAddr and Addr" (propIsBootstrapRedeemer @(Crypto era))
        ],
      TQC.testProperty "WitVKey does not brake containers due to invalid Ord" $
        propWitVKeys @(Crypto era)
    ]

-- | 'TestTree' of property-based testing properties.
propertyTests ::
  forall era ledger.
  ( EraGen era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    QC.HasTrace ledger (GenEnv era),
    Embed (Core.EraRule "DELEGS" era) ledger,
    Embed (Core.EraRule "UTXOW" era) ledger,
    State (Core.EraRule "PPUP" era) ~ PPUPState era,
    Environment ledger ~ LedgerEnv era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "update" (Core.TxBody era) (StrictMaybe (Update era)),
    HasField "addrWits" (Core.Witnesses era) (Set (WitVKey 'Witness (Crypto era))),
    HasField "scriptWits" (Core.Witnesses era) (Map (ScriptHash (Crypto era)) (Core.Script era)),
    QC.BaseEnv ledger ~ Globals,
    BaseM ledger ~ ReaderT Globals Identity,
    State ledger ~ (UTxOState era, DPState (Crypto era)),
    Signal ledger ~ Core.Tx era
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
        ],
      testGroup
        "STS Rules - Delegation Properties"
        [ TQC.testProperty
            "properties of the DELEG STS"
            (delegProperties @era)
        ],
      testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "properties of the POOL STS"
            (poolProperties @era)
        ],
      testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            (removedAfterPoolreap @era)
        ],
      testGroup
        "CHAIN level Properties"
        [ TQC.testProperty
            "collection of Ada preservation properties"
            (adaPreservationChain @era @ledger),
          TQC.testProperty
            "inputs are eliminated, outputs added to utxo and TxIds are unique"
            (collisionFreeComplete @era @ledger)
        ],
      testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            (onlyValidLedgerSignalsAreGenerated @era @ledger),
          TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            (onlyValidChainSignalsAreGenerated @era)
        ],
      testGroupByronTranslation,
      testGroupShelleyTranslation,
      testGroup
        "Compact Address Tests"
        [ TQC.testProperty "Compact address round trip" (propCompactAddrRoundTrip @(Crypto era)),
          TQC.testProperty "Compact address binary representation" (propCompactSerializationAgree @(Crypto era)),
          TQC.testProperty "determining address type doesn't force contents" (propDecompactAddrLazy @(Crypto era)),
          TQC.testProperty "reading the keyhash doesn't force the stake reference" (propDecompactShelleyLazyAddr @(Crypto era)),
          TQC.testProperty "isBootstrapRedeemer is equivalent for CompactAddr and Addr" (propIsBootstrapRedeemer @(Crypto era))
        ]
    ]
