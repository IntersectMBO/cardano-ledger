{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.PropertyTests (propertyTests, minimalPropertyTests) where

import Data.Proxy
import Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( bootstrapHashTest,
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
import Test.Shelley.Spec.Ledger.Serialisation.StakeRef
  ( propDeserializeAddrStakeReference,
    propDeserializeAddrStakeReferenceShort,
  )
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

proxyC :: Proxy C
proxyC = Proxy

minimalPropertyTests :: TestTree
minimalPropertyTests =
  testGroup
    "Minimal Property Tests"
    [ TQC.testProperty "Chain and Ledger traces cover the relevant cases" relevantCasesAreCovered,
      TQC.testProperty "total amount of Ada is preserved (Chain)" adaPreservationChain,
      TQC.testProperty "Only valid CHAIN STS signals are generated" onlyValidChainSignalsAreGenerated,
      bootstrapHashTest,
      testGroup
        "Deserialize stake address reference"
        [ TQC.testProperty "wstake reference from bytestrings" (propDeserializeAddrStakeReference @C),
          TQC.testProperty "stake reference from short bytestring" (propDeserializeAddrStakeReferenceShort @C)
        ],
      TQC.testProperty "legacy overlay schedule" (legacyOverlayTest proxyC)
    ]

-- | 'TestTree' of property-based testing properties.
propertyTests :: TestTree
propertyTests =
  testGroup
    "Property-Based Testing"
    [ testGroup
        "Classify Traces"
        [TQC.testProperty "Chain and Ledger traces cover the relevant cases" relevantCasesAreCovered],
      testGroup
        "STS Rules - Delegation Properties"
        [ TQC.testProperty
            "properties of the DELEG STS"
            delegProperties
        ],
      testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "properties of the POOL STS"
            poolProperties
        ],
      testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            removedAfterPoolreap
        ],
      testGroup
        "CHAIN level Properties"
        [ TQC.testProperty
            "collection of Ada preservation properties"
            adaPreservationChain,
          TQC.testProperty
            "inputs are eliminated, outputs added to utxo and TxIds are unique"
            collisionFreeComplete
        ],
      testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            onlyValidLedgerSignalsAreGenerated,
          TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            onlyValidChainSignalsAreGenerated
        ],
      testGroupByronTranslation
    ]
