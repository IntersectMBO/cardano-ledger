{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

-- import Cardano.Ledger.Babbage.Core

import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (TranslatableGen (..))
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

-- | Generates golden translation file for Conway era
main :: IO ()
main = generateGoldenFile @Conway [PlutusV1, PlutusV2, PlutusV3] "eras/conway/test-suite/golden/translations.cbor"

instance TranslatableGen Conway where
  tgTx l = BabbageTranslatableGen.genTx @Conway l (genTxBody l)
  tgUtxo = BabbageTranslatableGen.utxoWithTx @Conway

genTxBody :: forall era. Crypto era => Language -> Gen (ConwayTxBody (ConwayEra era))
genTxBody l = do
  let genTxOuts =
        fromList
          <$> listOf1
            ( mkSized (eraProtVerLow @Conway)
                <$> BabbageTranslatableGen.genTxOut @(ConwayEra era) l
            )
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn era))
  ConwayTxBody
    <$> genTxIns
    <*> arbitrary
    <*> ( case l of ----refinputs
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
