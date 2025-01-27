{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.Conway.Experiment.Ledger where

import Constrained.Experiment.API

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Test.Cardano.Ledger.Constrained.Conway.Experiment.Utxo

ledgerTxSpec ::
  UtxoExecContext ConwayEra ->
  Specification (Tx ConwayEra)
ledgerTxSpec = utxoTxSpec
