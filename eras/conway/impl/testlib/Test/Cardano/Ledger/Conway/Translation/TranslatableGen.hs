{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Translation.TranslatableGen where

import Cardano.Ledger.Alonzo.Plutus.Context (SupportedLanguage (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript, AsIx (..), PlutusPurpose)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Conway (ConwayEra, Tx (..))
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxBody (TxBody (ConwayTxBody))
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (Data (..), ExUnits, Language (..), plutusLanguage)
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (fromList)
import qualified Data.Set as Set
import Test.Cardano.Data.Arbitrary (genOSet)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (TranslatableGen (..))
import qualified Test.Cardano.Ledger.Babbage.Translation.TranslatableGen as BabbageTranslatableGen (
  genTx,
  genTxOut,
  utxoWithTx,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

instance TranslatableGen ConwayEra where
  tgRedeemers = genRedeemers
  tgTx = fmap MkConwayTx . BabbageTranslatableGen.genTx @ConwayEra . genTxBody
  tgUtxo = BabbageTranslatableGen.utxoWithTx @ConwayEra

genTxBody :: SupportedLanguage ConwayEra -> Gen (TxBody ConwayEra)
genTxBody l@(SupportedLanguage slang) = do
  let lang = plutusLanguage slang
      genTxOuts =
        fromList
          <$> listOf1
            ( mkSized (eraProtVerLow @ConwayEra)
                <$> BabbageTranslatableGen.genTxOut @ConwayEra l
            )
      genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen TxIn)
      offPrePlutusV3 freq = if lang >= PlutusV3 then freq else 0
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
        case lang of
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
