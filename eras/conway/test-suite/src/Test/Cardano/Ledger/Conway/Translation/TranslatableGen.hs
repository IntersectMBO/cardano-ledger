{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Translation.TranslatableGen where

import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, AsIx (..), PlutusPurpose)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Plutus (Data (..), ExUnits, Language (..), SLanguage (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Test.Cardano.Data.Arbitrary (genOSet)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  TxInfoLanguage (..),
 )
import qualified Test.Cardano.Ledger.Babbage.Translation.TranslatableGen as BabbageTranslatableGen (
  genTx,
  genTxOut,
  utxoWithTx,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

instance TranslatableGen Conway where
  tgRedeemers = genRedeemers
  tgTx l = BabbageTranslatableGen.genTx @Conway (genTxBody l)
  tgUtxo = BabbageTranslatableGen.utxoWithTx @Conway
  mkTxInfoLanguage PlutusV1 = TxInfoLanguage SPlutusV1
  mkTxInfoLanguage PlutusV2 = TxInfoLanguage SPlutusV2
  mkTxInfoLanguage PlutusV3 = TxInfoLanguage SPlutusV3

genTxBody :: forall c. Crypto c => Language -> Gen (ConwayTxBody (ConwayEra c))
genTxBody l = do
  let genTxOuts =
        fromList
          <$> listOf1
            ( mkSized (eraProtVerLow @Conway)
                <$> BabbageTranslatableGen.genTxOut @(ConwayEra c) l
            )
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn c))
      offPrePlutusV3 freq = if l >= PlutusV3 then freq else 0
      genDelegatee =
        frequency
          [ (33, DelegStake <$> arbitrary)
          , (offPrePlutusV3 33, DelegVote <$> arbitrary)
          , (offPrePlutusV3 33, DelegStakeVote <$> arbitrary <*> arbitrary)
          ]
      genDelegCert =
        frequency
          [ (25, ConwayRegCert <$> arbitrary <*> arbitrary)
          , (25, ConwayUnRegCert <$> arbitrary <*> arbitrary)
          , (25, ConwayDelegCert <$> arbitrary <*> genDelegatee)
          , (offPrePlutusV3 25, ConwayRegDelegCert <$> arbitrary <*> genDelegatee <*> arbitrary)
          ]
      genTxCerts =
        genOSet $
          frequency
            [ (33, ConwayTxCertDeleg <$> genDelegCert)
            , (33, ConwayTxCertPool <$> arbitrary)
            , (offPrePlutusV3 33, ConwayTxCertGov <$> arbitrary)
            ]
      genForPlutusV3 :: Arbitrary a => a -> Gen a
      genForPlutusV3 d =
        case l of
          PlutusV3 -> arbitrary
          _ -> pure d
  ConwayTxBody
    <$> genTxIns
    <*> arbitrary
    <*> arbitrary
    <*> genTxOuts
    <*> arbitrary
    <*> arbitrary
    <*> genTxCerts
    <*> arbitrary
    <*> arbitrary
    <*> scale (`div` 15) arbitrary
    <*> arbitrary
    <*> scale (`div` 15) arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genForPlutusV3 (VotingProcedures mempty)
    <*> genForPlutusV3 mempty
    <*> genForPlutusV3 mempty
    <*> genForPlutusV3 mempty

genRedeemers ::
  forall era.
  (AlonzoEraScript era, PlutusPurpose AsIx era ~ ConwayPlutusPurpose AsIx era) =>
  Gen (Redeemers era)
genRedeemers = do
  d <- arbitrary :: Gen (Data era)
  eu <- arbitrary :: Gen ExUnits
  -- We provide `RdrmPtr Spend 0` as the only valid reedemer, because
  -- for any other redeemer type, we would have to modify the body of the transaction
  -- in order for the translation to succeed
  Redeemers <$> elements [Map.singleton (ConwaySpending $ AsIx 0) (d, eu), Map.empty]
