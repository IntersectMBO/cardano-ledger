{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.Conway.Ledger where

import Cardano.Ledger.Shelley.API.Types

import Constrained

import Cardano.Ledger.Conway (ConwayEra, Conway)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Utxo

ledgerTxSpec ::
  IsConwayUniv fn =>
  UtxoExecContext Conway ->
  LedgerEnv (ConwayEra StandardCrypto) ->
  LedgerState (ConwayEra StandardCrypto) ->
  Specification fn (Tx (ConwayEra StandardCrypto))
ledgerTxSpec ctx env st =
  constrained $ \tx ->
    [ satisfies tx (utxoTxSpec ctx utxoEnv (lsUTxOState st))
    ]
  where
    utxoEnv =
      UtxoEnv
        { ueSlot = ledgerSlotNo env
        , uePParams = ledgerPp env
        , ueCertState = lsCertState st
        }
