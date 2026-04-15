{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Leios (tests) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Binary (DecCBOR (..), decodeFullAnnotator)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core (eraProtVerHigh)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import qualified Data.ByteString.Lazy as BSL
import Data.Either (isRight)
import Paths_cardano_ledger_alonzo_test (getDataFileName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Leios"
    [ testCase "backwards-compatible block decoder accepts pre-Leios Alonzo block" $ do
        -- The Alonzo golden block was encoded before the leios block format change.
        -- It uses the old encoding: a flat CBOR list of [header, txBodies, txWits, txMeta, txScripts]
        -- (5 elements = 1 + numSegComponents @AlonzoEra) rather than the new leios encoding:
        -- [header, body, mayAnnouncedEb, certifiesEb] (always 4 elements).
        filePath <- getDataFileName "golden/block.cbor"
        bytes <- BSL.readFile filePath
        assertBool "Failed to decode pre-Leios Alonzo block with backwards-compat decoder" $
          isRight $
            decodeFullAnnotator @(Block (BHeader StandardCrypto) AlonzoEra)
              (eraProtVerHigh @AlonzoEra)
              "Alonzo Block"
              decCBOR
              bytes
    ]
