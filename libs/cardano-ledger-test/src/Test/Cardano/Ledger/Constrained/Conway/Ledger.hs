{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.Conway.Ledger where

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Utxo

ledgerTxSpec ::
  IsConwayUniv fn =>
  UtxoExecContext ConwayEra ->
  Specification fn (Tx ConwayEra)
ledgerTxSpec = utxoTxSpec
