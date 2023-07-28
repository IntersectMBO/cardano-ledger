{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Translation.TranslatableGen where

import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (TranslatableGen (..))
import qualified Test.Cardano.Ledger.Babbage.Translation.TranslatableGen as BabbageTranslatableGen (
  genTx,
  genTxOut,
  utxoWithTx,
 )
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (
  Gen,
  arbitrary,
  listOf1,
  scale,
 )

instance TranslatableGen Conway where
  tgTx l = BabbageTranslatableGen.genTx @Conway (genTxBody l)
  tgUtxo = BabbageTranslatableGen.utxoWithTx @Conway

genTxBody :: forall c. Crypto c => Language -> Gen (ConwayTxBody (ConwayEra c))
genTxBody l = do
  let genTxOuts =
        fromList
          <$> listOf1
            ( mkSized (eraProtVerLow @Conway)
                <$> BabbageTranslatableGen.genTxOut @(ConwayEra c) l
            )
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn c))
  ConwayTxBody
    <$> genTxIns
    <*> arbitrary
    <*> ( case l of -- refinputs
            PlutusV1 -> pure Set.empty
            _ -> arbitrary
        )
    <*> genTxOuts
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> scale (`div` 15) arbitrary
    <*> arbitrary
    <*> scale (`div` 15) arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
