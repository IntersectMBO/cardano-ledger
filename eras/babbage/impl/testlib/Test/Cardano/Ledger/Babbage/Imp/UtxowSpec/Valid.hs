{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid (spec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Shelley.Scripts (pattern RequireAnyOf)
import Cardano.Ledger.TxIn (mkTxInPartial)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Valid" $ do
  it "Native reference scripts must be witnessed" $ do
    addr <- freshKeyAddr_
    let
      timelock = fromNativeScript @era $ RequireAnyOf []
      txOut =
        mkCoinTxOut addr (inject $ Coin 15_000_000)
          & referenceScriptTxOutL .~ SJust timelock
    tx0 <-
      submitTx $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ [txOut]
    let
      txIn = mkTxInPartial (txIdTx tx0) 0
      tx1 =
        mkBasicTx mkBasicTxBody
          & bodyTxL . referenceInputsTxBodyL .~ [txIn]
    submitTx_ tx1
