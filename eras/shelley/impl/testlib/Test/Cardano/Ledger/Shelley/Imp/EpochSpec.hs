{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Test.Cardano.Ledger.Common (Spec, describe, shouldBe)
import Test.Cardano.Ledger.Shelley.ImpTest (
  EraImpTest,
  getsNES,
  impIO,
  itM,
  passEpoch,
  submitTx,
 )

spec ::
  forall era.
  EraImpTest era =>
  Spec
spec = describe "EPOCH" $ do
  itM @era "Runs basic transaction" $ do
    do
      certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
      govState <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL
      impIO $ totalObligation certState govState `shouldBe` zero
    do
      deposited <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosDepositedL
      impIO $ deposited `shouldBe` zero
    _ <- submitTx "simple transaction" $ mkBasicTx mkBasicTxBody
    passEpoch

  itM @era "Crosses the epoch boundary" $ do
    do
      epoch <- getsNES nesELL
      impIO $ epoch `shouldBe` 0
    passEpoch
    do
      epoch <- getsNES nesELL
      impIO $ epoch `shouldBe` 1
