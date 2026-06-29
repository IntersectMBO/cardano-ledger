{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Plutus (mkInlineDatum)
import qualified Data.ByteString as BS
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Babbage.ImpTest (
  AlonzoEraImp,
  ImpInit,
  LedgerSpec,
  freshKeyAddr_,
  getsPParams,
  submitFailingTx,
 )
import Test.Cardano.Ledger.Common (SpecWith, describe, it)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  -- TxOut too large for the included ADA, using a large inline datum
  it "Min-utxo value with output too large" $ do
    pp <- getsPParams id
    addr <- freshKeyAddr_
    let
      amount = inject $ Coin 5_000_000
      largeDatum = PV1.B $ BS.replicate 1500 0
      txOut = mkBasicTxOut addr amount & datumTxOutL .~ mkInlineDatum largeDatum
    submitFailingTx
      (mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut])
      [ injectFailure $
          BabbageOutputTooSmallUTxO [(txOut, getMinCoinTxOut pp txOut)]
      ]
