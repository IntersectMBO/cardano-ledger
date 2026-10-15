{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.TickSpec (
  spec,
) where

import Cardano.Ledger.Shelley.LedgerState (nesLastTickSlotNoL)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "TICK" $ do
  it "Sets the last slot number when ticking" $ do
    slotNo <- getCurSlotNo
    passTick
    getsNES nesLastTickSlotNoL `shouldReturn` slotNo
