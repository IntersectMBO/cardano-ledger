{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Translation.Golden (
  generateGoldenFile,
  assertTranslationResultsMatchGolden,
) where

import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..))
import Cardano.Ledger.Binary.Encoding (serialize)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Language (Language)
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity (Identity)
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (TranslatableGen (..), epochInfo, systemStart, translationInstances)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  deserializeTranslationInstances,
 )
import Test.QuickCheck (Arbitrary)
import Test.Tasty.HUnit (Assertion, assertEqual)

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
  let instances = translationInstances @era ls 100 100000 -- 100 instances with an arbitrary seed
  let cbor = serialize (eraProtVerHigh @era) instances
  BSL.writeFile file cbor

assertTranslationResultsMatchGolden ::
  forall era.
  ( EraTx era
  , ExtendedUTxO era
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
  ( EraTx era
  , ExtendedUTxO era
  , HasCallStack
  ) =>
  TranslationInstance era ->
  Assertion
assertTranslationComparison (TranslationInstance pp l utxo tx expected) =
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
        Right actual -> assertEqual errorMessage expected actual
