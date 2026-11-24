{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core (
  AlonzoEraTxWits (..),
  DijkstraEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  MaryEraTxBody (..),
  Tx,
  TxLevel (..),
  ppMaxTxExUnitsL,
 )
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  ExUnits (..),
  Language (..),
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad ((>=>))
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Common (SpecWith, describe, forM_, it)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  ImpInit,
  ImpTestM,
  LedgerSpec,
  fixupPPHash,
  freshKeyAddrNoPtr_,
  getsPParams,
  impGetUTxO,
  rederiveAddrTxWits,
  submitPhase2Invalid_,
  submitTxAnn,
  submitTxAnn_,
  withPostFixup,
 )
import Test.Cardano.Ledger.Imp.Common (choose)
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysSucceedsNoDatum,
  inputSpendingGuard,
  purposeIsWellformedNoDatum,
 )

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "Plutus" $ do
    forM_ [l | l <- eraLanguages @era, l >= PlutusV4] $ \lang ->
      withSLanguage lang $ \slang -> do
        describe "purposeIsWellformedNoDatum" $ do
          it "Passes with guarding purpose" $ do
            let sh = hashPlutusScript $ purposeIsWellformedNoDatum slang
            submitTxAnn_ "Submit tx with a Plutus guard" $
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ OSet.singleton (ScriptHashObj sh)
        -- Pre-Dijkstra, transactions which need to perform translation level
        -- validation (such as batcher transactions which process multiple order
        -- utxos in a single transaction) would use the withdraw-zero trick to
        -- perform tx level validation in order to save ExBudget.
        -- Dijkstra onwards, guard scripts can perform tx-level validation.
        -- This is better than the withdraw-zero trick because we don't need to
        -- perform stake credential registration and provide a deposit.
        --
        -- This scenario submits a tx which spends multiple script utxos that
        -- contain a specific protocol token. The script locking those utxos is
        -- also their guard: when spending, it only forwards validation to its
        -- guard execution, which verifies that each script utxo has the
        -- specific protocol token.

        -- This scenario shows the delegation mechanism, not a secure tx
        -- batcher. For example, a real batcher guard would also check that each
        -- order's owner is paid, in one pass over the inputs and outputs
        -- instead of one per spending script (which is why the withdraw-zero
        -- trick was used).
        describe "inputSpendingGuard" $ do
          let inputSpendingGuardScriptHash = hashPlutusScript $ inputSpendingGuard slang

              submitOrders :: Int -> Int -> ImpTestM era [TxIn]
              submitOrders nbOrdersWithToken nbOrdersWithoutToken = do
                let
                  protocolPolicyId = PolicyID $ hashPlutusScript $ alwaysSucceedsNoDatum slang
                  mkProtocolTokenMultiAsset = MultiAsset . Map.singleton protocolPolicyId . Map.singleton (AssetName "protocol")
                  mkOrderTxOut = mkBasicTxOut (mkAddr inputSpendingGuardScriptHash StakeRefNull) . MaryValue mempty
                  orderTxOuts =
                    replicate nbOrdersWithToken (mkOrderTxOut $ mkProtocolTokenMultiAsset 1)
                      ++ replicate nbOrdersWithoutToken (mkOrderTxOut mempty)
                tx <-
                  submitTxAnn "Create order UTxOs" $
                    mkBasicTx mkBasicTxBody
                      & bodyTxL . mintTxBodyL .~ mkProtocolTokenMultiAsset (toInteger nbOrdersWithToken)
                      & bodyTxL . outputsTxBodyL .~ SSeq.fromList orderTxOuts
                pure [txInAt i tx | i <- [0 .. length orderTxOuts - 1]]

              mkOrderProcessingTx :: [TxIn] -> ImpTestM era (Tx TopTx era)
              mkOrderProcessingTx orderTxIns = do
                orderValues <- mapM (fmap (^. valueTxOutL) . impGetUTxO) orderTxIns
                protocolAddr <- freshKeyAddrNoPtr_
                pp <- getsPParams id
                pure $
                  mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL .~ Set.fromList orderTxIns
                    -- A single order's ADA can be below the min ADA of the protocol address
                    & bodyTxL . outputsTxBodyL .~ [ensureMinCoinTxOut pp . mkBasicTxOut protocolAddr $ fold orderValues]
                    & bodyTxL . guardsTxBodyL .~ OSet.singleton (ScriptHashObj inputSpendingGuardScriptHash)

              -- The ExUnits of a failing script can't be estimated, so the fixup assigns it the whole
              -- `ppMaxTxExUnitsL`, which exceeds the tx limit as soon as any other script runs.
              -- Share the limit equally between all redeemers instead, which is still far more than
              -- these scripts need, so they can only fail on their own logic.
              submitPhase2InvalidSharingExUnits = do
                let
                  shareMaxTxExUnits :: Tx TopTx era -> ImpTestM era (Tx TopTx era)
                  shareMaxTxExUnits tx = do
                    ExUnits mem steps <- getsPParams ppMaxTxExUnitsL
                    let redeemers = tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL
                        n = fromIntegral $ Map.size redeemers
                        sharedExUnits = ExUnits (mem `div` n) (steps `div` n)
                    pure $
                      tx
                        & witsTxL . rdmrsTxWitsL . unRedeemersL
                          .~ fmap (\(dat, _) -> (dat, sharedExUnits)) redeemers
                withPostFixup (shareMaxTxExUnits >=> fixupPPHash >=> rederiveAddrTxWits)
                  . submitPhase2Invalid_
          it "Batches orders validated once by the guard" $ do
            nbOrdersWithToken <- choose (1, 10)
            orderTxIns <- submitOrders nbOrdersWithToken 0
            mkOrderProcessingTx orderTxIns >>= submitTxAnn_ "Submit batch tx with the guard inputSpendingGuard"
          it "Fails when the guard is missing" $ do
            nbOrdersWithToken <- choose (1, 10)
            orderTxIns <- submitOrders nbOrdersWithToken 0
            orderProcessingTx <- mkOrderProcessingTx orderTxIns
            submitPhase2InvalidSharingExUnits $ orderProcessingTx & bodyTxL . guardsTxBodyL .~ mempty
          it "Fails when an order lacks the protocol token" $ do
            nbOrdersWithToken <- choose (1, 10)
            nbOrdersWithNoToken <- choose (1, 3)
            orderTxIns <- submitOrders nbOrdersWithToken nbOrdersWithNoToken
            mkOrderProcessingTx orderTxIns >>= submitPhase2InvalidSharingExUnits
