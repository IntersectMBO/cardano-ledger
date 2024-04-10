{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.Conway.Ledger where

import Cardano.Ledger.Shelley.API.Types

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Utxo

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
