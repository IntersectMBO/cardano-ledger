{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.V2.Conway.LEDGER where

import Cardano.Ledger.Api
import Cardano.Ledger.Shelley.API.Types

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway
import Test.Cardano.Ledger.Constrained.V2.Conway.UTXO

ledgerTxSpec ::
  ConwayUniverse fn =>
  LedgerEnv (ConwayEra StandardCrypto) ->
  LedgerState (ConwayEra StandardCrypto) ->
  Spec fn (Tx (ConwayEra StandardCrypto))
ledgerTxSpec env st =
  constrained $ \tx ->
    [ satisfies tx (utxoTxSpec utxoEnv (lsUTxOState st))
    ]
  where
    utxoEnv =
      UtxoEnv
        { ueSlot = ledgerSlotNo env
        , uePParams = ledgerPp env
        , ueCertState = lsCertState st
        }
