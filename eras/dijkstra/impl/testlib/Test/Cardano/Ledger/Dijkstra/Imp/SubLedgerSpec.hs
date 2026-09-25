{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubLedgerSpec (spec) where

import Cardano.Ledger.BaseTypes (Mismatch (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraSubLedgerPredFailure (..))
import Cardano.Ledger.State (treasuryL)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBLEDGER" $ do
  describe "SubTreasuryValueMismatch" $ do
    it "a sub-transaction declares a treasury value other than the actual one" $ do
      actualTreasury <- getsNES treasuryL
      let declaredTreasury = actualTreasury <> Coin 1
      submitFailingSubTx
        (declareTreasurySubTx declaredTreasury)
        [ injectFailure . SubTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredTreasury
              , mismatchExpected = actualTreasury
              }
        ]

    it "every sub-transaction is checked against the same treasury value" $ do
      actualTreasury <- getsNES treasuryL
      let declaredFirst = actualTreasury <> Coin 1
          declaredSecond = actualTreasury <> Coin 2
      submitFailingTx
        (mkTopTxWithSubTxs [declareTreasurySubTx declaredFirst, declareTreasurySubTx declaredSecond])
        [ injectFailure . SubTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredFirst
              , mismatchExpected = actualTreasury
              }
        , injectFailure . SubTreasuryValueMismatch $
            Mismatch
              { mismatchSupplied = declaredSecond
              , mismatchExpected = actualTreasury
              }
        ]

  describe "Accepted at the boundary" $ do
    it "a sub-transaction declares the actual treasury value" $ do
      actualTreasury <- getsNES treasuryL
      submitTx_ . mkTopTxWithSubTxs $ [declareTreasurySubTx actualTreasury]

    it "a treasury donation in an earlier sub-transaction does not change the value checked" $ do
      actualTreasury <- getsNES treasuryL
      let donatingSubTx :: Tx SubTx era
          donatingSubTx = mkBasicTx $ mkBasicTxBody & treasuryDonationTxBodyL .~ Coin 1_000
      submitTx_ . mkTopTxWithSubTxs $ [donatingSubTx, declareTreasurySubTx actualTreasury]

  describe "A phase-2 invalid top level transaction" $
    disableInConformanceIt "raises no SUBLEDGER failure" $ do
      actualTreasury <- getsNES treasuryL
      topTx <-
        phase2InvalidTx . mkTopTxWithSubTxs $
          [declareTreasurySubTx $ actualTreasury <> Coin 1]
      withNoFixup $ submitTx_ topTx
