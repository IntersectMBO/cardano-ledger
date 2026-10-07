{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.ReceivingFixupSpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (unRedeemersL, unTxDatsL)
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxoPredFailure (NoCollateralInputs))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  SLanguage (..),
  hashPlutusScript,
  pointWiseExUnits,
 )
import Cardano.Ledger.State (UTxO (..), utxoL)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum, alwaysSucceedsWithDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "Receiving transaction fixups" $ do
  it "generates valid child-only Receiving with body-local budgets and one collateral input" $ do
    first <- receivingChild
    second <- receivingChild
    fixed <- submitTx (mkTopTxWithSubTxs [first, second])
    limit <- getsPParams ppMaxTxExUnitsL
    assertBool "Batch budget exceeds the protocol limit" $
      pointWiseExUnits (<=) (getTotalExUnits fixed) limit
    assertBool "Top-level body unexpectedly acquired a redeemer" $
      null (fixed ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
    Set.size (fixed ^. bodyTxL . collateralInputsTxBodyL) `shouldBe` 1
    let children = OMap.elems (fixed ^. bodyTxL . subTransactionsTxBodyL)
    length children `shouldBe` 2
    forM_ children $ \child -> do
      Map.keys (child ^. witsTxL . rdmrsTxWitsL . unRedeemersL) `shouldBe` [ReceivingPurpose (AsIx 0)]
      assertBool "Child is missing its integrity hash" $
        child ^. bodyTxL . scriptIntegrityHashTxBodyL /= SNothing

  it "creates separate child redeemers for byte-identical protected outputs" $ do
    child <- receivingChild
    let duplicateOutputs =
          child
            & bodyTxL . outputsTxBodyL %~ \outputs ->
              let halves = fmap (coinTxOutL @era %~ \(Coin amount) -> Coin (amount `div` 2)) outputs
               in halves <> halves
    fixed <- submitTx (mkTopTxWithSubTxs [duplicateOutputs])
    case OMap.elems (fixed ^. bodyTxL . subTransactionsTxBodyL) of
      [fixedChild] -> do
        Map.keys (fixedChild ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
          `shouldBe` [ReceivingPurpose (AsIx 0), ReceivingPurpose (AsIx 1)]
        case SSeq.lookup 0 (fixedChild ^. bodyTxL . outputsTxBodyL) of
          Just firstOutput -> SSeq.lookup 1 (fixedChild ^. bodyTxL . outputsTxBodyL) `shouldBe` Just firstOutput
          Nothing -> assertFailure "Missing duplicated child output"
      _ -> assertFailure "Expected one child transaction"

  it "keeps the same raw index in two child domains with independent authored redeemers and budgets" $ do
    first <- receivingChild
    second <- receivingChild
    let pointer = ReceivingPurpose (AsIx 0)
        firstValue = (Data (P.I 2), ExUnits 1_000_000 100_000_000)
        secondValue = (Data (P.I 4), ExUnits 2_000_000 200_000_000)
        authored child value = child & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.singleton pointer value
    fixed <- submitTx (mkTopTxWithSubTxs [authored first firstValue, authored second secondValue])
    [ Map.lookup pointer (child ^. witsTxL . rdmrsTxWitsL . unRedeemersL)
      | child <- OMap.elems (fixed ^. bodyTxL . subTransactionsTxBodyL)
      ]
      `shouldMatchList` [Just firstValue, Just secondValue]

  it "allows a post-fixup missing-collateral test to stay invalid" $ do
    child <- receivingChild
    let removeCollateral =
          rederiveAddrTxWits
            . (bodyTxL . collateralInputsTxBodyL .~ mempty)
            . (bodyTxL . collateralReturnTxBodyL .~ SNothing)
            . (bodyTxL . totalCollateralTxBodyL .~ SNothing)
    (failures, _) <-
      withPostFixup removeCollateral $
        expectLeftDeepExpr =<< trySubmitTx (mkTopTxWithSubTxs [child])
    assertBool "Removing batch collateral did not report NoCollateralInputs" $
      injectFailure (NoCollateralInputs @era) `elem` failures

  it "preserves a test-supplied child redeemer and budget" $ do
    child <- receivingChild
    let pointer = ReceivingPurpose (AsIx 0)
        supplied = (Data (P.I 9), ExUnits 1 1)
        authored = child & witsTxL . rdmrsTxWitsL . unRedeemersL .~ Map.singleton pointer supplied
    fixed <- fixupSubTransactions (mkTopTxWithSubTxs [authored])
    case OMap.elems (fixed ^. bodyTxL . subTransactionsTxBodyL) of
      [fixedChild] ->
        Map.lookup pointer (fixedChild ^. witsTxL . rdmrsTxWitsL . unRedeemersL) `shouldBe` Just supplied
      _ -> assertFailure "Expected one child transaction"

  it "discovers spending datums at a protected script destination" $ do
    let scriptHash = hashPlutusScript (alwaysSucceedsWithDatum SPlutusV4)
    input <- produceScript scriptHash
    -- Trusted fixture construction isolates spending datum discovery from the
    -- separate Receiving authorization needed for production output creation.
    modifyNES $
      utxoL %~ \(UTxO entries) ->
        UTxO $
          Map.adjust
            (addrTxOutL .~ AddrProtected Testnet (ScriptHashObj scriptHash) StakeRefNull)
            input
            entries
    fixed <- submitTx (mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [input]))
    assertBool "Protected spending input lost its required datum witness" $
      not (Map.null (fixed ^. witsTxL . datsTxWitsL . unTxDatsL))

receivingChild :: DijkstraEraImp era => ImpTestM era (Tx SubTx era)
receivingChild = do
  amount <- Coin . fromIntegral <$> choose (4_000_000 :: Int, 8_000_000)
  fundingAddr <- freshKeyAddrNoPtr_
  input <- sendCoinTo fundingAddr amount
  let scriptHash = hashPlutusScript (alwaysSucceedsNoDatum SPlutusV4)
      protected = AddrProtected Testnet (ScriptHashObj scriptHash) StakeRefNull
  pure $
    mkBasicTx $
      mkBasicTxBody
        & inputsTxBodyL .~ [input]
        & outputsTxBodyL .~ [mkCoinTxOut protected amount]
