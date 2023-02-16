module Test.Cardano.Ledger.Shelley.ShelleyTranslation (testGroupShelleyTranslation) where

import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (totalAdaES)
import Cardano.Ledger.Shelley.LedgerState (EpochState, returnRedeemAddrsToReserves)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Protocol.TPraos.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Protocol.TPraos.ShelleyEraGen ()
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
