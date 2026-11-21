{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Tx.ImpSpec (spec) where

import Cardano.Ledger.Api.Tx (producedTxOuts)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.State (UTxO (..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Lens.Micro
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

-- Take the snapshot after fixup, which can submit transactions to fund inputs.
checkProducedOutputs :: HasCallStack => Tx TopTx DijkstraEra -> ImpTestM DijkstraEra (UTxO DijkstraEra)
checkProducedOutputs tx = do
  fixed <- fixupTx tx
  UTxO utxoBefore <- getUTxO
  submitted <- withNoFixup $ submitTx fixed
  UTxO utxoAfter <- getUTxO
  let added = UTxO (utxoAfter `Map.difference` utxoBefore)
  producedTxOuts submitted `shouldBe` added
  pure added

batchWithOutputs :: ImpTestM DijkstraEra (Tx TopTx DijkstraEra)
batchWithOutputs = do
  topOut <- freshTxOutWithCoin (Coin 3_000_000)
  firstOut <- freshTxOutWithCoin (Coin 4_000_000)
  secondOut <- freshTxOutWithCoin (Coin 5_000_000)
  otherOut <- freshTxOutWithCoin (Coin 6_000_000)
  pure $
    mkTopTxWithSubTxs
      [ mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [firstOut, secondOut]
      , mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [otherOut]
      ]
      & bodyTxL . outputsTxBodyL .~ [topOut]

spec :: Spec
spec = withImpInitEachEraVersion (Proxy @DijkstraEra) $
  describe "producedTxOuts" $ do
    it "matches ledger additions for a batch with multiple sub-transactions" $ do
      added <- checkProducedOutputs =<< batchWithOutputs
      Map.size (unUTxO added) `shouldSatisfy` (>= 4)
    it "matches ledger additions without sub-transactions" $ do
      out <- freshTxOutWithCoin (Coin 3_000_000)
      added <- checkProducedOutputs $ mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [out]
      Map.size (unUTxO added) `shouldSatisfy` (> 0)
    it "returns only the collateral return for a phase-2-invalid batch" $ do
      tx <- switchTxToPhase2InvalidLegacyMode =<< batchWithOutputs
      collateralAddr <- freshKeyAddrNoPtr_
      let collateralReturn = mkBasicTxOut collateralAddr mempty & coinTxOutL .~ Coin 3_000_000
      added <-
        checkProducedOutputs $
          tx
            & isPhase2ValidTxL .~ Phase2Invalid
            & bodyTxL . collateralReturnTxBodyL .~ SJust collateralReturn
      Map.size (unUTxO added) `shouldBe` 1
    it "returns no outputs for a phase-2-invalid batch without a collateral return" $ do
      tx <- switchTxToPhase2InvalidLegacyMode =<< batchWithOutputs
      added <- checkProducedOutputs $ tx & isPhase2ValidTxL .~ Phase2Invalid
      added `shouldBe` mempty
