{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.ByronTranslation (testGroupByronTranslation) where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.ByronTranslation
import Cardano.Ledger.Shelley.TxBody
import Test.Cardano.Chain.UTxO.Gen (genCompactTxOut)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.QuickCheck.Hedgehog (hedgehog)
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
  translateTxOutByronToShelley
    @C_Crypto
    (Byron.fromCompactTxOut compactTxOut)
    === translateCompactTxOutByronToShelley compactTxOut

{------------------------------------------------------------------------------
  Reference implementation
------------------------------------------------------------------------------}

translateTxOutByronToShelley ::
  forall crypto.
  CryptoClass.Crypto crypto =>
  Byron.TxOut ->
  TxOut (ShelleyEra crypto)
translateTxOutByronToShelley (Byron.TxOut addr amount) =
  TxOut (translateAddr addr) (translateAmount amount)
  where
    translateAmount :: Byron.Lovelace -> Coin
    translateAmount = Coin . Byron.lovelaceToInteger

    translateAddr :: Byron.Address -> Addr crypto
    translateAddr = AddrBootstrap . BootstrapAddress

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

instance Arbitrary Byron.CompactTxOut where
  arbitrary = hedgehog genCompactTxOut
