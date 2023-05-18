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

import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..), VersionedTxInfo)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..))
import Cardano.Ledger.Binary.Encoding (serialize)
import Cardano.Ledger.Core
import Cardano.Ledger.Language (Language)
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity (Identity)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (TranslatableGen (..), epochInfo, systemStart, translationInstances)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  deserializeTranslationInstances,
 )
import Test.QuickCheck (Arbitrary)

-- | Generates arguments for `ExtendedUTxO.txInfo`, applies them to it
-- and serializes both arguments and result to golden/translations.cbor file
generateGoldenFile ::
  forall era.
  ( ExtendedUTxO era
  , TranslatableGen era
  , Arbitrary (PParamsHKD Identity era)
  ) =>
  [Language] ->
  FilePath ->
  IO ()
generateGoldenFile ls file = do
  putStrLn $ "Generating golden file for TxInfo: " <> file
  instances <- translationInstances @era ls 100
  let cbor = serialize (eraProtVerHigh @era) instances
  BSL.writeFile file cbor

compareGoldenTxInfoResults ::
  forall era.
  ( EraTx era
  , ExtendedUTxO era
  ) =>
  IO FilePath ->
  IO [TxInfoResultComparison era]
compareGoldenTxInfoResults file = do
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
    checkTranslationInstance (TranslationInstance pp l utxo tx expected) =
      let result = txInfo pp l epochInfo systemStart utxo tx
          errorMessage =
            unlines
              [ "Unexpected txinfo with arguments: "
              , " pp: " <> show pp
              , " language: " <> show l
              , " utxo: " <> show utxo
              , " tx: " <> show tx
              ]
       in case result of
            Left e -> error $ show e
            Right info -> TxInfoResultComparison expected info errorMessage
