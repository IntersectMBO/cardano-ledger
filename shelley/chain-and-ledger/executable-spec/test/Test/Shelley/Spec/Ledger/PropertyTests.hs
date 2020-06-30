{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Shelley.Spec.Ledger.PropertyTests (propertyTests, minimalPropertyTests) where

import Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( bootstrapHashTest,
  )
import Test.Shelley.Spec.Ledger.Rules.ClassifyTraces
  ( onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    relevantCasesAreCovered,
  )
import Test.Shelley.Spec.Ledger.Rules.TestChain
  ( constantSumPots,
    nonNegativeDeposits,
    adaPreservationChain,
    rewardStkCredSync,
    removedAfterPoolreap,
  )
import Test.Shelley.Spec.Ledger.Rules.TestLedger
  ( consumedEqualsProduced,
    credentialMappingAfterDelegation,
    credentialRemovedAfterDereg,
    eliminateTxInputs,
    feesNonDecreasing,
    newEntriesAndUniqueTxIns,
    noDoubleSpend,
    pStateIsInternallyConsistent,
    poolIsMarkedForRetirement,
    poolRetireInEpoch,
    potsSumIncreaseWdrls,
    preserveBalance,
    preserveBalanceRestricted,
    preserveOutputsTx,
    prop_MIRValuesEndUpInMap,
    prop_MIRentriesEndUpInMap,
    registeredPoolIsAdded,
    rewardZeroAfterRegKey,
    rewardZeroAfterRegPool,
    rewardsDecreasesByWithdrawals,
    rewardsSumInvariant,
  )
import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

minimalPropertyTests :: TestTree
minimalPropertyTests =
  testGroup
    "Minimal Property Tests"
    [ TQC.testProperty "Chain and Ledger traces cover the relevant cases" relevantCasesAreCovered,
      TQC.testProperty "total amount of Ada is preserved (Chain)" adaPreservationChain,
      TQC.testProperty "reward and stake credential maps stay in sync" rewardStkCredSync,
      TQC.testProperty "Only valid CHAIN STS signals are generated" onlyValidChainSignalsAreGenerated,
      bootstrapHashTest
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
        [ TQC.testProperty "newly registered key has a reward of 0" rewardZeroAfterRegKey,
          TQC.testProperty "deregistered key's credential is removed" credentialRemovedAfterDereg,
          TQC.testProperty "registered stake credential is correctly delegated" credentialMappingAfterDelegation,
          TQC.testProperty "sum of rewards does not change" rewardsSumInvariant,
          TQC.testProperty "rewards pot decreases by the sum of tx withdrawals" rewardsDecreasesByWithdrawals
        ],
      testGroup
        "STS Rules - Utxo Properties"
        [ TQC.testProperty "the value consumed by UTXO is equal to the value produced in DELEGS" consumedEqualsProduced,
          TQC.testProperty "transaction fees are non-decreasing" feesNonDecreasing,
          TQC.testProperty "sum of circulation, deposits and fees increases by the sum of tx withdrawals" potsSumIncreaseWdrls,
          TQC.testProperty "preserve the balance in a transaction" preserveBalance,
          TQC.testProperty "preserve tx balance restricted to TxIns and TxOuts" preserveBalanceRestricted,
          TQC.testProperty "preserve transaction outputs" preserveOutputsTx,
          TQC.testProperty "consumed inputs are eliminated" eliminateTxInputs,
          TQC.testProperty "new tx entries are included and all txIds are new" newEntriesAndUniqueTxIns,
          TQC.testProperty "no double spend" noDoubleSpend
        ],
      testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "newly registered stake pool is added to \
            \appropriate state mappings"
            registeredPoolIsAdded,
          TQC.testProperty
            "newly registered pool key is not in the retiring map"
            rewardZeroAfterRegPool,
          TQC.testProperty
            "retired stake pool is removed from \
            \appropriate state mappings and marked \
            \ for retiring"
            poolIsMarkedForRetirement,
          TQC.testProperty
            "pool state is internally consistent"
            pStateIsInternallyConsistent,
          TQC.testProperty
            "executing a pool retirement certificate adds to 'retiring'"
            poolRetireInEpoch
        ],
      testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "circulation+deposits+fees+treasury+rewards+reserves is constant."
            constantSumPots,
          TQC.testProperty
            "deposits are always non-negative"
            nonNegativeDeposits,
          TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            removedAfterPoolreap
        ],
      testGroup
        "STS Rules - NewEpoch Properties"
        [ TQC.testProperty
            "total amount of Ada is preserved"
            adaPreservationChain
        ],
      testGroup
        "STS Rules - MIR certificates"
        [ TQC.testProperty
            "entries of MIR certificate are added to\
            \ irwd mapping"
            prop_MIRentriesEndUpInMap,
          TQC.testProperty
            "coin values of entries of a MIR certificate\
            \ are added to the irwd mapping"
            prop_MIRValuesEndUpInMap
        ],
      testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            onlyValidLedgerSignalsAreGenerated,
          TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            onlyValidChainSignalsAreGenerated
        ]
    ]
