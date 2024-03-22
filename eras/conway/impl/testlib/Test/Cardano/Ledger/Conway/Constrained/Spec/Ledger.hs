{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Conway.Constrained.Spec.Ledger where

import Cardano.Ledger.Shelley.API.Types

import Constrained

import Test.Cardano.Ledger.Conway.Constrained.Instances
import Test.Cardano.Ledger.Conway.Constrained.Spec.Utxo
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Conway.Core

ledgerTxSpec ::
  IsConwayUniv fn =>
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
