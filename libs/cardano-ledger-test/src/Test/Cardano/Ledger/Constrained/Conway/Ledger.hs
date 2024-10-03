{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.Conway.Ledger where

import Constrained

import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Utxo

ledgerTxSpec ::
  IsConwayUniv fn =>
  UtxoExecContext Conway ->
  Specification fn (Tx (ConwayEra StandardCrypto))
ledgerTxSpec = utxoTxSpec
