{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Deserializes `TranslationInstance`s from golden/translations.cbor file.
--
-- Each instance represents arguments passed to `alonzoTxInfo` along with the produced result.
-- This test checks that calling `alonzoTxInfo` with the arguments from this file, produces the same result as in the flie.
--
-- To regenerate the golden file (for example, if the logic in the translation changes),
-- run the following command from the root of the repository:
-- cabal run cardano-ledger-alonzo-test:gen-golden"
module Test.Cardano.Ledger.Alonzo.GoldenTranslation (
  tests,
)
where

import Cardano.Ledger.Language (Language (..))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Alonzo.TxInfo (alonzoTxInfo)
import Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo_test
import Test.Cardano.Ledger.Alonzo.TranslationInstance (TranslationInstance (..), deserializeTranslationInstances, epochInfo, systemStart)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Golden translation tests"
    [ testCase "golden/translations.cbor" $ do
        tis <- readTranslationInstances
        checkTranslationInstances tis
    ]

readTranslationInstances :: IO [TranslationInstance]
readTranslationInstances = do
  bs <- getDataFileName "golden/translations.cbor" >>= BSL.readFile
  either throwIO pure (deserializeTranslationInstances bs)

checkTranslationInstances :: [TranslationInstance] -> Assertion
checkTranslationInstances = mapM_ checkTranslationInstance
  where
    checkTranslationInstance ti@(TranslationInstance pp l utxo tx expected) =
      let result = alonzoTxInfo pp l epochInfo systemStart utxo tx
       in case result of
            Left e -> error $ show e
            Right info -> assertEqual (errorMessage ti) info expected

errorMessage :: TranslationInstance -> String
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
