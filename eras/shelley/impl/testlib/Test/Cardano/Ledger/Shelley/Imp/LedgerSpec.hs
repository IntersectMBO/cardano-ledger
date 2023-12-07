{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp.LedgerSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes (TxIx (..), inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTx (..), EraTxBody (..), EraTxOut (..), coinTxOutL)
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosUtxoL,
 )
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.TxIn (TxIn (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

getUTxO :: ImpTestM era (UTxO era)
getUTxO = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL

spec ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpTestState era)
spec = describe "LEDGER" $ do
  it "Transactions update UTxO" $ do
    kpPayment1 <- lookupKeyPair =<< freshKeyHash
    kpStaking1 <- lookupKeyPair =<< freshKeyHash
    UTxO utxo0 <- getUTxO
    length utxo0 `shouldBe` 1
    let coin1 = Coin 1000
    txId1 <-
      submitTx "First transaction" . mkBasicTx $
        mkBasicTxBody
          & outputsTxBodyL @era
            .~ SSeq.singleton
              (mkBasicTxOut (mkAddr (kpPayment1, kpStaking1)) $ inject coin1)
    UTxO utxo1 <- getUTxO
    case Map.lookup (TxIn txId1 $ TxIx 1) utxo1 of
      Just out1 -> do
        out1 ^. coinTxOutL `shouldBe` coin1
      Nothing -> expectationFailure "Could not find the TxOut of the first transaction"
    kpPayment2 <- lookupKeyPair =<< freshKeyHash
    kpStaking2 <- lookupKeyPair =<< freshKeyHash
    let coin2 = Coin 500
    txId2 <-
      submitTx "Second transaction" . mkBasicTx $
        mkBasicTxBody
          & inputsTxBodyL
            .~ Set.singleton
              (TxIn txId1 $ TxIx 0)
          & outputsTxBodyL @era
            .~ SSeq.singleton
              (mkBasicTxOut (mkAddr (kpPayment2, kpStaking2)) $ inject coin2)
    UTxO utxo2 <- getUTxO
    case Map.lookup (TxIn txId2 $ TxIx 1) utxo2 of
      Just out1 -> do
        out1 ^. coinTxOutL `shouldBe` coin2
      Nothing -> expectationFailure "Could not find the TxOut of the second transaction"
