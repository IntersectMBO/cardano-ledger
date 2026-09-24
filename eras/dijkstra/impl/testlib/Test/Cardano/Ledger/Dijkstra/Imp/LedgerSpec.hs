{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LedgerSpec (spec) where

import Cardano.Ledger.BaseTypes (Mismatch (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (ppMaxRefScriptSizePerTxL)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.State (treasuryL)
import Cardano.Ledger.TxIn (TxIn, mkTxInPartial)
import qualified Data.OMap.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (purposeIsWellformedNoDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEDGER" $ do
  describe "Spending sub-transaction outputs" $ do
    it "Fails when top-level transaction spends output from its own sub-transaction" $ do
      txIn <- (`sendCoinTo` Coin 10_000_000) =<< freshKeyAddr_
      subTxIn <- (`sendCoinTo` Coin 5_000_000) =<< freshKeyAddr_

      let subTx :: Tx SubTx era
          -- consume an input, to avoid the fixup adding one, which would throw off the test conditions
          subTx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [subTxIn])
          subTxId = txIdTx subTx -- now stable through fixup
          badInput = mkTxInPartial subTxId 0
          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn, badInput]
              & bodyTxL . subTransactionsTxBodyL .~ [subTx]

      submitFailingTx
        tx
        -- the failure is produced twice - checking against the origin and threaded state, respectively
        [ injectFailure $ BadInputsUTxO $ NES.singleton badInput
        , injectFailure $ BadInputsUTxO $ NES.singleton badInput
        ]

    it "Fails when sub-transaction spends output from another sub-transaction" $ do
      (_, addr1) <- freshKeyAddr
      txIn1 <- sendCoinTo addr1 (Coin 10_000_000)
      (_, addr2) <- freshKeyAddr
      txIn2 <- sendCoinTo addr2 (Coin 10_000_000)

      let subTx1 :: Tx SubTx era
          subTx1 =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn1]
          subTx1Id = txIdTx subTx1

          badInput = mkTxInPartial subTx1Id 0
          subTx2 :: Tx SubTx era
          subTx2 =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ [txIn2, badInput]

          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . subTransactionsTxBodyL .~ [subTx1, subTx2]

      submitFailingTx
        tx
        [ injectFailure $ SubBadInputsUTxO $ NES.singleton badInput
        , injectFailure $ SubBadInputsUTxO $ NES.singleton badInput
        ]

    it "Succeeds when inputs don't reference sub-transaction outputs" $ do
      (_, addr1) <- freshKeyAddr
      txIn1 <- sendCoinTo addr1 (Coin 10_000_000)
      (_, addr2) <- freshKeyAddr
      txIn2 <- sendCoinTo addr2 (Coin 10_000_000)

      let subTx :: Tx SubTx era
          subTx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn1

          tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . inputsTxBodyL .~ Set.singleton txIn2
              & bodyTxL . subTransactionsTxBodyL .~ OMap.singleton subTx

      submitTx_ tx

  describe "DijkstraTreasuryValueMismatch" $ do
    it "the top level transaction declares a treasury value other than the actual one" $ do
      actualTreasury <- getsNES treasuryL
      let declaredTreasury = actualTreasury <> Coin 1
      submitFailingTx
        (mkBasicTx $ mkBasicTxBody & currentTreasuryValueTxBodyL .~ SJust declaredTreasury)
        [ injectFailure . DijkstraTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredTreasury
              , mismatchExpected = actualTreasury
              }
        ]

  describe "DijkstraTxRefScriptsSizeTooBig" $ do
    it "the reference scripts of a sub-transaction count towards the batch total" $ do
      (size, refTxIn) <- refScriptInput
      modifyPParams $ ppMaxRefScriptSizePerTxL .~ fromIntegral (size - 1)

      let subTx :: Tx SubTx era
          subTx = mkBasicTx $ mkBasicTxBody & referenceInputsTxBodyL .~ Set.singleton refTxIn
      submitFailingTx
        (mkTopTxWithSubTxs [subTx])
        [ injectFailure $
            DijkstraTxRefScriptsSizeTooBig
              Mismatch
                { mismatchSupplied = size
                , mismatchExpected = size - 1
                }
        ]

    it "a reference script shared by two sub-transactions is counted once for each" $ do
      (size, refTxIn) <- refScriptInput
      txIn1 <- (`sendCoinTo` Coin 10_000_000) =<< freshKeyAddr_
      txIn2 <- (`sendCoinTo` Coin 10_000_000) =<< freshKeyAddr_
      modifyPParams $ ppMaxRefScriptSizePerTxL .~ fromIntegral size

      let subTxSpending txIn =
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ Set.singleton txIn
                & referenceInputsTxBodyL .~ Set.singleton refTxIn
          subTxs :: [Tx SubTx era]
          subTxs = [subTxSpending txIn1, subTxSpending txIn2]
      submitFailingTx
        (mkTopTxWithSubTxs subTxs)
        [ injectFailure $
            DijkstraTxRefScriptsSizeTooBig
              Mismatch
                { mismatchSupplied = 2 * size
                , mismatchExpected = size
                }
        ]

  describe "Accepted at the boundary" $ do
    it "the top level transaction declares the actual treasury value" $ do
      actualTreasury <- getsNES treasuryL
      submitTx_ . mkBasicTx $
        mkBasicTxBody & currentTreasuryValueTxBodyL .~ SJust actualTreasury

    it "the reference scripts of the batch total exactly the limit" $ do
      (size, refTxIn) <- refScriptInput
      modifyPParams $ ppMaxRefScriptSizePerTxL .~ fromIntegral size

      submitTx_ . mkTopTxWithSubTxs . pure . mkBasicTx $
        mkBasicTxBody & referenceInputsTxBodyL .~ Set.singleton refTxIn

  describe "Composite tests" $ do
    it "both levels are checked, and the top level failure is reported first" $ do
      actualTreasury <- getsNES treasuryL
      let declaredInSubTx = actualTreasury <> Coin 1
          declaredInTopTx = actualTreasury <> Coin 2
      submitFailingTx
        ( mkTopTxWithSubTxs [declareTreasurySubTx declaredInSubTx]
            & bodyTxL . currentTreasuryValueTxBodyL .~ SJust declaredInTopTx
        )
        [ injectFailure . DijkstraTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredInTopTx
              , mismatchExpected = actualTreasury
              }
        , injectFailure . SubTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredInSubTx
              , mismatchExpected = actualTreasury
              }
        ]

    it "the reference script size is reported before the treasury value" $ do
      actualTreasury <- getsNES treasuryL
      (size, refTxIn) <- refScriptInput
      modifyPParams $ ppMaxRefScriptSizePerTxL .~ fromIntegral (size - 1)

      let declaredTreasury = actualTreasury <> Coin 1
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & currentTreasuryValueTxBodyL .~ SJust declaredTreasury
              & referenceInputsTxBodyL .~ Set.singleton refTxIn
        )
        [ injectFailure $
            DijkstraTxRefScriptsSizeTooBig
              Mismatch
                { mismatchSupplied = size
                , mismatchExpected = size - 1
                }
        , injectFailure . DijkstraTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredTreasury
              , mismatchExpected = actualTreasury
              }
        ]

  describe "A phase-2 invalid top level transaction" $ do
    it "is accepted when it exercises neither check" $ do
      topTx <- phase2InvalidTx $ mkBasicTx mkBasicTxBody
      withNoFixup $ submitTx_ topTx

    disableInConformanceIt "is not checked for the treasury value" $ do
      actualTreasury <- getsNES treasuryL
      topTx <-
        phase2InvalidTx . mkBasicTx $
          mkBasicTxBody & currentTreasuryValueTxBodyL .~ SJust (actualTreasury <> Coin 1)
      withNoFixup $ submitTx_ topTx

    disableInConformanceIt "is not checked for the reference script size" $ do
      (size, refTxIn) <- refScriptInput
      modifyPParams $ ppMaxRefScriptSizePerTxL .~ fromIntegral (size - 1)

      topTx <-
        phase2InvalidTx . mkBasicTx $
          mkBasicTxBody & referenceInputsTxBodyL .~ Set.singleton refTxIn
      withNoFixup $ submitTx_ topTx

-- | The size in bytes of a reference script, and an input that carries it.
refScriptInput :: forall era. DijkstraEraImp era => ImpTestM era (Int, TxIn)
refScriptInput = do
  plutusScript <- mkPlutusScript @era $ purposeIsWellformedNoDatum SPlutusV3
  let script = fromPlutusScript plutusScript
  refTxIn <- produceRefScript script
  pure (originalBytesSize script, refTxIn)
