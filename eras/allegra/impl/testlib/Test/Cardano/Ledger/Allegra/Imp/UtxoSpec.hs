{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Allegra.Rules
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Core
import Cardano.Slotting.Slot (SlotNo (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  AllegraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec =
  describe "UTXO" $ do
    describe "validity interval" $ do
      let situations x d =
            [ (lo, hi, loOk && hiOk)
            | (lo, loOk) <-
                [ (SNothing, True)
                , (SJust (x - d), True)
                , (SJust x, True)
                , (SJust (x + d), False)
                ]
            , (hi, hiOk) <-
                [ (SNothing, True)
                , (SJust (x - d), False)
                , (SJust x, False)
                , (SJust (x + d), True)
                ]
            ]
          test currentSlot d =
            forM_ (situations currentSlot d) $ \(lo, hi, expectedSuccess) -> do
              let validityInterval = ValidityInterval lo hi
                  tx = mkBasicTx $ mkBasicTxBody & vldtTxBodyL .~ validityInterval
              if expectedSuccess
                then
                  submitTx_ @era tx
                else
                  submitFailingTx
                    tx
                    [injectFailure $ OutsideValidityIntervalUTxO validityInterval currentSlot]
      it "corner cases around the current slot" $ do
        currentSlot <- getCurSlotNo
        test currentSlot 1

      it "cases around the current slot" $ do
        currentSlot <- getCurSlotNo
        d <- SlotNo <$> choose (2, unSlotNo currentSlot)
        test currentSlot d
