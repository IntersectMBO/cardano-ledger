{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Translation.Golden (
  generateGoldenFile,
  readGoldenFile,
  compareTxInfoResults,
  compareGoldenTxInfoResults,
  TxInfoResultComparison (..),
) where

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL

import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), VersionedTxInfo)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..))
import Cardano.Ledger.Core -- (eraProtVerHigh, Era, PParams, PParamsHKD, Tx, TxOut)
import Cardano.Ledger.Language (Language)
import Control.Exception (throwIO)
import Data.Functor.Identity (Identity)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  deserializeTranslationInstances,
 )
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (TranslatableGen (..), epochInfo, systemStart, translationInstances)
import Test.QuickCheck (Arbitrary)

-- | Generates arguments for `ExtendedUTxO.txInfo`, applies them to it
-- and serializes both arguments and result to golden/translations.cbor file
generateGoldenFile ::
  forall era.
  ( ExtendedUTxO era
  , TranslatableGen era
  , Arbitrary (PParams era)
  ) =>
  [Language] ->
  FilePath ->
  IO ()
generateGoldenFile ls file =
  do
    putStrLn $ "Generating golden file for TxInfo: " <> file
    instances <- translationInstances @era 100 ls
    let cbor = serialize (eraProtVerHigh @era) instances
    BSL.writeFile file cbor

compareGoldenTxInfoResults ::
  forall era.
  ( EraTx era
  , ExtendedUTxO era
  ) =>
  IO FilePath ->
  IO [TxInfoResultComparison era]
compareGoldenTxInfoResults file =
  do
    instances <- readGoldenFile @era file
    pure $ compareTxInfoResults @era instances

readGoldenFile ::
  forall era.
  ( Era era
  , DecCBOR (PParamsHKD Identity era)
  , DecCBOR (TxOut era)
  , DecCBOR (Annotator (Tx era))
  ) =>
  IO FilePath ->
  IO [TranslationInstance era]
readGoldenFile file = do
  bs <- file >>= BSL.readFile
  either throwIO pure (deserializeTranslationInstances bs)

data TxInfoResultComparison era = TxInfoResultComparison
  { tiExpected :: VersionedTxInfo
  , tiActual :: VersionedTxInfo
  , tiErrorMessage :: String
  }

compareTxInfoResults ::
  forall era.
  ( EraTx era
  , ExtendedUTxO era
  ) =>
  [TranslationInstance era] ->
  [TxInfoResultComparison era]
compareTxInfoResults = map checkTranslationInstance
  where
    checkTranslationInstance ti@(TranslationInstance pp l utxo tx expected) =
      let result = txInfo pp l epochInfo systemStart utxo tx
       in case result of
            Left e -> error $ show e
            Right info -> TxInfoResultComparison expected info (errorMessage ti)
    errorMessage (TranslationInstance pp l utxo tx _) =
      "Unexpected txinfo with arguments: "
        <> "\n pp: "
        <> show pp
        <> "\n language: "
        <> show l
        <> "\n utxo: "
        <> show utxo
        <> "\n tx: "
        <> show tx
