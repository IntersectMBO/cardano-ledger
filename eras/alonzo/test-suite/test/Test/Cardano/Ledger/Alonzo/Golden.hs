{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Golden Tests for the Alonzo era
module Test.Cardano.Ledger.Alonzo.Golden (
  tests,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Tx (alonzoMinFeeTx)
import Cardano.Ledger.BaseTypes (boundRational)
import Cardano.Ledger.Binary (decCBOR, decodeFullAnnotator)
import Cardano.Ledger.Block (Block (Block))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Plutus.ExUnits (
  Prices (..),
 )
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import qualified Data.ByteString.Base16.Lazy as B16L
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import Data.Sequence.Strict
import Lens.Micro
import Paths_cardano_ledger_alonzo_test
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Binary.Golden (cborAnnGoldenSpec)
import Test.Cardano.Ledger.Common hiding (output)
import Test.Cardano.Protocol.TPraos.Examples (
  ProtocolLedgerExamples (..),
  ledgerExamplesAlonzo,
 )

readDataFile :: FilePath -> IO BSL.ByteString
readDataFile name = getDataFileName name >>= BSL.readFile

tests :: Spec
tests =
  describe "Alonzo Golden Tests" $ do
    goldenCborSerialization
    goldenMinFee

goldenCborSerialization :: Spec
goldenCborSerialization =
  describe "Golden tests - CBOR serialization" $ do
    cborAnnGoldenSpec
      getDataFileName
      "golden/block.cbor"
      (eraProtVerLow @AlonzoEra)
      (pleBlock ledgerExamplesAlonzo)

goldenMinFee :: Spec
goldenMinFee =
  describe "golden tests - minimum fee calculation" $ do
    it "Alonzo Block" $ do
      -- This golden test uses the block from:
      -- https://github.com/input-output-hk/cardano-node/issues/4228#issuecomment-1195707491
      --
      -- The first transaction in this block is invalid due to:
      --   FeeTooSmallUTxO (Coin 1006053) (Coin 1001829)
      --
      -- The correct behavior is for the minimum fee for this transaction
      -- to be 1006053 lovelace, as indicated by the failure above.
      -- Nodes that had the bug determined the minimum fee to be 1001829.
      hex <- readDataFile "golden/hex-block-node-issue-4228.cbor"
      let cborBytesBlock =
            case B16L.decode hex of
              Left err -> error err
              Right val -> val
          blockBody =
            case decodeFullAnnotator (eraProtVerHigh @AlonzoEra) "Block" decCBOR cborBytesBlock of
              Left err -> error (show err)
              Right (Block _bHeader bBody :: Block (BHeader StandardCrypto) AlonzoEra) -> bBody
          firstTx =
            case blockBody ^. txSeqBlockBodyL of
              tx :<| _ -> (tx :: Tx TopTx AlonzoEra)
              Empty -> error "Block doesn't have any transactions"

          -- Below are the relevant protocol parameters that were active
          -- at the time this block was rejected.
          priceMem = fromJust $ boundRational 0.0577
          priceSteps = fromJust $ boundRational 0.0000721
          pricesParam = Prices priceMem priceSteps
          pp =
            emptyPParams
              & ppTxFeePerByteL .~ CoinPerByte (CompactCoin 44)
              & ppTxFeeFixedL .~ Coin 155381
              & ppPricesL .~ pricesParam

      Coin 1006053 `shouldBe` alonzoMinFeeTx pp firstTx
