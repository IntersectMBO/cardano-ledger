{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LedgersSpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules (ConwayLedgersPredFailure (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppMaxRefScriptSizePerBlockL)
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  OrdExUnits (..),
  SLanguage (..),
  hashPlutusScript,
 )
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.TxIn (TxIn)
import Data.List.NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((%~), (&), (.~))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEDGERS" $ do
  it "BodyRefScriptsSizeTooBig" $ do
    script <- RequireSignature @era <$> freshKeyHash
    let refScriptCount = 3
    -- Two transactions, each referencing the script 3 times between itself and its sub-transaction
    txs <- mkRefScriptBatchTxs 2 refScriptCount (fromNativeScript script)
    -- we are setting the limit to half of what we provide
    let blockLimit = refScriptCount * originalBytesSize script
    modifyPParams $ ppMaxRefScriptSizePerBlockL .~ fromIntegral blockLimit
    submitFailingBlock
      txs
      [ injectFailure $
          BodyRefScriptsSizeTooBig
            Mismatch
              { -- both transactions are applied, the second one exceeding the limit
                mismatchSupplied = 2 * blockLimit
              , mismatchExpected = blockLimit
              }
      ]

  it "BodyRefScriptsSizeTooBig with reference scripts in the same block" $ do
    script <- RequireSignature @era <$> freshKeyHash
    let
      refScriptCount = 3
      blockLimit = refScriptCount * originalBytesSize script
    -- we are setting the limit to half of what we provide
    modifyPParams $ ppMaxRefScriptSizePerBlockL .~ fromIntegral blockLimit
    withTxsInFailingBlock
      (mapM_ submitTx_ =<< mkRefScriptBatchTxs 2 refScriptCount (fromNativeScript script))
      [ injectFailure $
          BodyRefScriptsSizeTooBig
            Mismatch
              { mismatchSupplied = 2 * blockLimit
              , mismatchExpected = blockLimit
              }
      ]

  it "A block exactly at the refScriptSize limit is accepted" $ do
    script <- RequireSignature @era <$> freshKeyHash
    let
      refScriptCount = 3
      blockLimit = refScriptCount * originalBytesSize script
    -- this time we are providing exactly the limit
    txs <- mkRefScriptBatchTxs 1 refScriptCount (fromNativeScript script)
    modifyPParams $ ppMaxRefScriptSizePerBlockL .~ fromIntegral blockLimit
    withTxsInBlock_ $ mapM_ submitTx_ txs

  it "TooManyExUnits" $ do
    maxTxUnits@(ExUnits maxTxMem maxTxSteps) <- getsPParams ppMaxTxExUnitsL
    let scriptUnits = ExUnits (maxTxMem `div` 2) (maxTxSteps `div` 2)

    -- Two transactions, each running two scripts: one in itself and one in its sub-transaction, which
    -- share the per-transaction budget between them
    txs <- replicateM 2 $ mkExUnitsBatchTx scriptUnits
    -- The block may hold one such transaction, and we provide two
    modifyPParams $ ppMaxBlockExUnitsL .~ maxTxUnits
    submitFailingBlock
      txs
      [ injectFailure $
          TooManyExUnits
            Mismatch
              { -- twice the limit: two transactions, each using up a whole per-transaction budget
                mismatchSupplied = OrdExUnits $ ExUnits (maxTxMem * 2) (maxTxSteps * 2)
              , mismatchExpected = OrdExUnits maxTxUnits
              }
      ]

  it "A block exactly at the exUnits limit is accepted" $ do
    maxTxUnits@(ExUnits maxTxMem maxTxSteps) <- getsPParams ppMaxTxExUnitsL
    let scriptUnits = ExUnits (maxTxMem `div` 2) (maxTxSteps `div` 2)
    tx <- mkExUnitsBatchTx scriptUnits
    -- this time we are providing exactly the limit
    modifyPParams $ ppMaxBlockExUnitsL .~ maxTxUnits
    withTxsInBlock_ $ submitTx_ tx

  it "Transactions after the first failure are not applied" $ do
    script <- RequireSignature @era <$> freshKeyHash
    let refScriptCount = 3
    -- Three transactions, each referencing the script `refScriptCount` times between itself and its sub-transaction
    txs <- mkRefScriptBatchTxs 3 refScriptCount (fromNativeScript script)
    -- the limit allows one transaction, so the second one already exceeds it
    let blockLimit = refScriptCount * originalBytesSize script
    modifyPParams $ ppMaxRefScriptSizePerBlockL .~ fromIntegral blockLimit
    submitFailingBlock
      txs
      [ injectFailure $
          BodyRefScriptsSizeTooBig
            Mismatch
              { -- Only the first two were applied, the second one exceeding the limit.
                -- The third one never contributes, since LEDGERS stops at the first failure.
                mismatchSupplied = 2 * blockLimit
              , mismatchExpected = blockLimit
              }
      ]
  where
    -- Create the given number of transactions, each referencing the same script the given number of times
    -- across the top-level transaction and its sub-transaction
    mkRefScriptBatchTxs :: Int -> Int -> Script era -> ImpTestM era [Tx TopTx era]
    mkRefScriptBatchTxs txCount refScriptCountPerTx script =
      replicateM txCount $ do
        txIn <- freshKeyAddr_ >>= \addr -> sendCoinTo addr (Coin 100_000_000)
        topRefIn :| subRefIns <-
          produceRefScripts (script :| replicate (refScriptCountPerTx - 1) script)
        pure $ mkRefScriptBatchTx (topRefIn, subRefIns) & bodyTxL . inputsTxBodyL .~ [txIn]

    -- A transaction that references the script once itself and the rest of the times in a
    -- sub-transaction. All of them are counted, even though they hold the same script.
    mkRefScriptBatchTx :: (TxIn, [TxIn]) -> Tx TopTx era
    mkRefScriptBatchTx (topRefIn, subRefIns) =
      mkTopTxWithSubTxs
        [mkBasicTx mkBasicTxBody & bodyTxL . referenceInputsTxBodyL .~ Set.fromList subRefIns]
        & bodyTxL . referenceInputsTxBodyL .~ [topRefIn]

    -- A transaction that spends one script-locked input itself and another in a
    -- sub-transaction. Setting both redeemers explicitly keeps the fixup from estimating
    -- them, so the batch is charged the whole per-transaction budget.
    mkExUnitsBatchTx :: ExUnits -> ImpTestM era (Tx TopTx era)
    mkExUnitsBatchTx scriptUnits = do
      topTxIn <- produceScript $ hashPlutusScript (alwaysSucceedsWithDatum SPlutusV4)
      subTxIn <- produceScript $ hashPlutusScript (alwaysSucceedsWithDatum SPlutusV4)
      let subTx = spendUsingExUnits scriptUnits subTxIn $ mkBasicTx mkBasicTxBody
      pure $ spendUsingExUnits scriptUnits topTxIn $ mkTopTxWithSubTxs [subTx]

    spendUsingExUnits :: ExUnits -> TxIn -> Tx l era -> Tx l era
    spendUsingExUnits exUnits txIn tx =
      tx
        & bodyTxL . inputsTxBodyL .~ [txIn]
        & witsTxL . rdmrsTxWitsL . unRedeemersL
          %~ Map.insert (mkSpendingPurpose (AsIx 0)) (Data (P.I 0), exUnits)
