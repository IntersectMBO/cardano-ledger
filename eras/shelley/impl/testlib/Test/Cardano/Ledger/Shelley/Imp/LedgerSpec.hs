{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp.LedgerSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes (inject)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEDGER" $ do
  it "Transactions update UTxO" $ do
    addr1 <- freshKeyAddr_
    let coin1 = Coin 2000000
    tx1 <-
      submitTopTxAnn "First transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr1 $ inject coin1)
    UTxO utxo1 <- getUTxO
    case Map.lookup (txInAt 0 tx1) utxo1 of
      Just out1 -> out1 ^. coinTxOutL `shouldBe` coin1
      Nothing -> expectationFailure "Could not find the TxOut of the first transaction"
    addr2 <- freshKeyAddr_
    let coin2 = Coin 3000000
    tx2 <-
      submitTopTxAnn "Second transaction" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton (txInAt 0 tx1)
          & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr2 $ inject coin2)
    UTxO utxo2 <- getUTxO
    case Map.lookup (txInAt 0 tx2) utxo2 of
      Just out1 -> do
        out1 ^. coinTxOutL `shouldBe` coin2
      Nothing -> expectationFailure "Could not find the TxOut of the second transaction"
