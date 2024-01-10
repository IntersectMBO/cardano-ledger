{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.EpochSpec (
  spec,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEsL,
  totalObligation,
  utxosDepositedL,
  utxosGovStateL,
 )
import Cardano.Ledger.Val (Val (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (
  ImpTestState,
  ShelleyEraImp,
  getsNES,
  passEpoch,
  submitTxAnn_,
 )

spec ::
  forall era.
  ShelleyEraImp era =>
  SpecWith (ImpTestState era)
spec = describe "EPOCH" $ do
  it "Runs basic transaction" $ do
    do
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      govState <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
      totalObligation certState govState `shouldBe` zero
    do
      deposited <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
      deposited `shouldBe` zero
    submitTxAnn_ "simple transaction" $ mkBasicTx mkBasicTxBody
    passEpoch

  it "Crosses the epoch boundary" $ do
    getsNES nesELL `shouldReturn` 0
    passEpoch
    getsNES nesELL `shouldReturn` 1
