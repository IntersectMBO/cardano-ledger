{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.TxInfoSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core (
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
 )
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusTxInfo (..), LedgerTxInfo (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus (SLanguage (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: Spec
spec = withImpInit @(LedgerSpec AlonzoEra) $ describe "TxInfo" $ do
  describe "PlutusV1" $ do
    it "toPlutusTxInfo does not fail when Byron scripts are present in TxOuts" $ do
      pv <- getProtVer
      Globals {epochInfo, systemStart} <- use impGlobalsL
      (_, shelleyAddr) <- freshKeyAddr
      byronAddr <- AddrBootstrap <$> freshBootstapAddress
      shelleyTxIn <- sendCoinTo shelleyAddr mempty
      utxo <- getUTxO
      let
        byronTxOut = mkBasicTxOut byronAddr . inject $ Coin 1
        tx =
          mkBasicTx @AlonzoEra mkBasicTxBody
            & bodyTxL
              . inputsTxBodyL
              .~ Set.singleton shelleyTxIn
            & bodyTxL
              . outputsTxBodyL
              .~ SSeq.singleton byronTxOut
        lti =
          LedgerTxInfo
            { ltiProtVer = pv
            , ltiEpochInfo = epochInfo
            , ltiSystemStart = systemStart
            , ltiUTxO = utxo
            , ltiTx = tx
            }
      void $ expectRight $ toPlutusTxInfo SPlutusV1 lti
    it "toPlutusTxInfo does not fail when Byron scripts are present in TxIns" $ do
      pv <- getProtVer
      Globals {epochInfo, systemStart} <- use impGlobalsL
      (_, shelleyAddr) <- freshKeyAddr
      byronAddr <- AddrBootstrap <$> freshBootstapAddress
      byronTxIn <- sendCoinTo byronAddr mempty
      utxo <- getUTxO
      let
        shelleyTxOut = mkBasicTxOut shelleyAddr . inject $ Coin 1
        tx =
          mkBasicTx @AlonzoEra mkBasicTxBody
            & bodyTxL
              . inputsTxBodyL
              .~ Set.singleton byronTxIn
            & bodyTxL
              . outputsTxBodyL
              .~ SSeq.singleton shelleyTxOut
        lti =
          LedgerTxInfo
            { ltiProtVer = pv
            , ltiEpochInfo = epochInfo
            , ltiSystemStart = systemStart
            , ltiUTxO = utxo
            , ltiTx = tx
            }
      void $ expectRight $ toPlutusTxInfo SPlutusV1 lti
