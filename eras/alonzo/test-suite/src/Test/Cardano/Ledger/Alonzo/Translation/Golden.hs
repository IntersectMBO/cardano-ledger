{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Translation.Golden (
  generateGoldenFile,
  assertTranslationResultsMatchGolden,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError, LedgerTxInfo (..), toPlutusTxInfo)
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript)
import Cardano.Ledger.Binary.Encoding (serialize)
import Cardano.Ledger.Core
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  TxInfoLanguage (..),
  epochInfo,
  systemStart,
  toVersionedTxInfo,
  translationInstances,
 )
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  deserializeTranslationInstances,
 )
import Test.Tasty.HUnit (Assertion, assertEqual)

-- | Generates arguments for `ExtendedUTxO.txInfo`, applies them to it
-- and serializes both arguments and result to golden/translations.cbor file
generateGoldenFile ::
  forall era.
  ( Show (ContextError era)
  , AlonzoEraScript era
  , TranslatableGen era
  ) =>
  FilePath ->
  IO ()
generateGoldenFile file = do
  putStrLn $ "Generating golden file for TxInfo: " <> file
  let instances = translationInstances @era 100 100000 -- 100 instances with an arbitrary seed
  let cbor = serialize (eraProtVerHigh @era) instances
  BSL.writeFile file cbor

assertTranslationResultsMatchGolden ::
  forall era.
  ( TranslatableGen era
  , Show (ContextError era)
  , HasCallStack
  ) =>
  IO FilePath ->
  Assertion
assertTranslationResultsMatchGolden file = do
  bs <- file >>= BSL.readFile
  instances <- either throwIO pure (deserializeTranslationInstances @era bs)
  mapM_ assertTranslationComparison instances

assertTranslationComparison ::
  forall era.
  ( TranslatableGen era
  , Show (ContextError era)
  , HasCallStack
  ) =>
  TranslationInstance era ->
  Assertion
assertTranslationComparison (TranslationInstance protVer lang utxo tx expected) =
  case mkTxInfoLanguage @era lang of
    TxInfoLanguage slang -> do
      case toPlutusTxInfo slang lti of
        Left e -> error $ show e
        Right actual -> assertEqual errorMessage expected $ toVersionedTxInfo slang actual
  where
    lti =
      LedgerTxInfo
        { ltiProtVer = protVer
        , ltiEpochInfo = epochInfo
        , ltiSystemStart = systemStart
        , ltiUTxO = utxo
        , ltiTx = tx
        }
    errorMessage =
      unlines
        [ "Unexpected txinfo with arguments: "
        , " ProtVer: " <> show protVer
        , " language: " <> show lang
        , " utxo: " <> show utxo
        , " tx: " <> show tx
        ]
