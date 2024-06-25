{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babel.Translation.TranslatableGen where

import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, AsIx (..), PlutusPurpose)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Babel (Babel, BabelEra)
import Cardano.Ledger.Babel.Scripts (BabelPlutusPurpose (..))
import Cardano.Ledger.Babel.TxBody (BabelTxBody (..))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus (Data (..), ExUnits, Language (..), SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  TxInfoLanguage (..),
 )
import qualified Test.Cardano.Ledger.Babbage.Translation.TranslatableGen as BabbageTranslatableGen (
  genTx,
  genTxOut,
  utxoWithTx,
 )
import Test.Cardano.Ledger.Babel.Arbitrary ()
import Test.Cardano.Ledger.Common

instance TranslatableGen Babel where
  tgRedeemers = genRedeemers
  tgTx l = BabbageTranslatableGen.genTx @Babel (genTxBody l)
  tgUtxo = BabbageTranslatableGen.utxoWithTx @Babel
  mkTxInfoLanguage PlutusV1 = TxInfoLanguage SPlutusV1
  mkTxInfoLanguage PlutusV2 = TxInfoLanguage SPlutusV2
  mkTxInfoLanguage PlutusV3 = TxInfoLanguage SPlutusV3
  mkTxInfoLanguage PlutusV4 = TxInfoLanguage SPlutusV4

genTxBody :: forall c. Crypto c => Language -> Gen (BabelTxBody (BabelEra c))
genTxBody l = do
  let genTxOuts =
        fromList
          <$> listOf1
            ( mkSized (eraProtVerLow @Babel)
                <$> BabbageTranslatableGen.genTxOut @(BabelEra c) l
            )
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn c))
  BabelTxBody
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
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genRedeemers ::
  forall era.
  (AlonzoEraScript era, PlutusPurpose AsIx era ~ BabelPlutusPurpose AsIx era) =>
  Gen (Redeemers era)
genRedeemers = do
  d <- arbitrary :: Gen (Data era)
  eu <- arbitrary :: Gen ExUnits
  -- We provide `RdrmPtr Spend 0` as the only valid reedemer, because
  -- for any other redeemer type, we would have to modify the body of the transaction
  -- in order for the translation to succeed
  Redeemers <$> elements [Map.singleton (BabelSpending $ AsIx 0) (d, eu), Map.empty]
