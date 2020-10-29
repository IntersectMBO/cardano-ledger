module Test.Shelley.Spec.Ledger.ShelleyTranslation (testGroupShelleyTranslation) where

import Shelley.Spec.Ledger.ShelleyEra (ShelleyEra)
import Shelley.Spec.Ledger.LedgerState (EpochState, returnRedeemAddrsToReserves)
import Shelley.Spec.Ledger.STS.Chain (totalAdaES)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

testGroupShelleyTranslation :: TestTree
testGroupShelleyTranslation =
  testGroup
    "Translation from Shelley to Allegra"
    [ testProperty "returning redeemers preserves ada" propRemoveRedeemPreservesAda
    ]

propRemoveRedeemPreservesAda ::
  EpochState (ShelleyEra C_Crypto) -> Property
propRemoveRedeemPreservesAda es =
  totalAdaES es === (totalAdaES . returnRedeemAddrsToReserves) es
