module Test.Cardano.Ledger.Shelley.ByronTranslation (testGroupByronTranslation) where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.ByronTranslation
import Cardano.Ledger.Shelley.Core
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.QuickCheck ((===))
import Test.Tasty
import Test.Tasty.QuickCheck

{------------------------------------------------------------------------------
  Top-level tests
------------------------------------------------------------------------------}

testGroupByronTranslation :: TestTree
testGroupByronTranslation =
  testGroup
    "Translation from Byron to Shelley"
    [ testProperty "translateTxOut correctness" prop_translateTxOut_correctness
    ]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

prop_translateTxOut_correctness :: Byron.CompactTxOut -> Property
prop_translateTxOut_correctness compactTxOut =
  translateTxOutByronToShelley (Byron.fromCompactTxOut compactTxOut)
    === translateCompactTxOutByronToShelley compactTxOut

{------------------------------------------------------------------------------
  Reference implementation
------------------------------------------------------------------------------}

translateTxOutByronToShelley :: Byron.TxOut -> TxOut ShelleyEra
translateTxOutByronToShelley (Byron.TxOut addr amount) =
  mkBasicTxOut (translateAddr addr) (translateAmount amount)
  where
    translateAmount :: Byron.Lovelace -> Coin
    translateAmount = Coin . Byron.lovelaceToInteger

    translateAddr :: Byron.Address -> Addr
    translateAddr = AddrBootstrap . BootstrapAddress
