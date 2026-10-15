{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.TickSpec (
  spec,
) where

import Cardano.Ledger.Shelley.LedgerState (nesLastBlockSlotNoL)
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "TICK" $ do
  it "Sets the last block slot number when ticking a slot" $ do
    slotNo <- use impCurSlotNoG
    passTick
    getsNES nesLastBlockSlotNoL `shouldReturn` slotNo

  it "Sets the last block slot number when crossing an epoch boundary" $ do
    passEpoch
    slotNo <- use impCurSlotNoG
    getsNES nesLastBlockSlotNoL `shouldReturn` pred slotNo
