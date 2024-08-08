{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Shelley.Imp.EpochSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), addEpochInterval)
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
import Test.Cardano.Ledger.Shelley.ImpTest

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

  it "Crosses epoch boundaries" $ do
    startEpochNo <- getsNES nesELL
    Positive n <- arbitrary
    passNEpochs $ fromIntegral n
    getsNES nesELL `shouldReturn` addEpochInterval startEpochNo (EpochInterval n)
