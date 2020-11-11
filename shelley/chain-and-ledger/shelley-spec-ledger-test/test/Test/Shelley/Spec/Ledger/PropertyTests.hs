{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Test.Shelley.Spec.Ledger.PropertyTests (propertyTests, minimalPropertyTests) where

import qualified Cardano.Ledger.Core as Core
import Data.Proxy
import Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( bootstrapHashTest,
  )
import Test.Shelley.Spec.Ledger.Address.CompactAddr
  ( propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactAddrLazy,
    propDecompactShelleyLazyAddr,
  )
import Test.Shelley.Spec.Ledger.ByronTranslation (testGroupByronTranslation)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.LegacyOverlay (legacyOverlayTest)
import Test.Shelley.Spec.Ledger.Rules.ClassifyTraces
  ( onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    relevantCasesAreCovered,
  )
import Test.Shelley.Spec.Ledger.Rules.TestChain
  ( adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    poolProperties,
    removedAfterPoolreap,
  )
import Test.Shelley.Spec.Ledger.ShelleyTranslation (testGroupShelleyTranslation)
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

proxyC :: Proxy C
proxyC = Proxy

minimalPropertyTests :: TQC.Gen (Core.Value C) -> TestTree
minimalPropertyTests gv =
  testGroup
    "Minimal Property Tests"
    [ TQC.testProperty "Chain and Ledger traces cover the relevant cases" (relevantCasesAreCovered gv),
      TQC.testProperty "total amount of Ada is preserved (Chain)" (adaPreservationChain gv),
      TQC.testProperty "Only valid CHAIN STS signals are generated" (onlyValidChainSignalsAreGenerated gv),
      bootstrapHashTest,
      testGroup
        "Compact Address Tests"
        [ TQC.testProperty "Compact address round trip" (propCompactAddrRoundTrip @C),
          TQC.testProperty "Compact address binary representation" (propCompactSerializationAgree @C),
          TQC.testProperty "determining address type doesn't force contents" (propDecompactAddrLazy @C),
          TQC.testProperty "reading the keyhash doesn't force the stake reference" (propDecompactShelleyLazyAddr @C)
        ],
      TQC.testProperty "legacy overlay schedule" (legacyOverlayTest proxyC)
    ]

-- | 'TestTree' of property-based testing properties.
propertyTests ::
  TQC.Gen (Core.Value C) -> TestTree
propertyTests gv =
  testGroup
    "Property-Based Testing"
    [ testGroup
        "Classify Traces"
        [TQC.testProperty "Chain and Ledger traces cover the relevant cases" (relevantCasesAreCovered gv)],
      testGroup
        "STS Rules - Delegation Properties"
        [ TQC.testProperty
            "properties of the DELEG STS"
            (delegProperties gv)
        ],
      testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "properties of the POOL STS"
            (poolProperties gv)
        ],
      testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            (removedAfterPoolreap gv)
        ],
      testGroup
        "CHAIN level Properties"
        [ TQC.testProperty
            "collection of Ada preservation properties"
            (adaPreservationChain gv),
          TQC.testProperty
            "inputs are eliminated, outputs added to utxo and TxIds are unique"
            (collisionFreeComplete gv)
        ],
      testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            (onlyValidLedgerSignalsAreGenerated gv),
          TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            (onlyValidChainSignalsAreGenerated gv)
        ],
      testGroupByronTranslation,
      testGroupShelleyTranslation
    ]
