{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.Conway.Ledger where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Constrained.API
import Test.Cardano.Ledger.Constrained.Conway.Utxo

ledgerTxSpec ::
  UtxoExecContext ConwayEra ->
  Specification (Tx ConwayEra)
ledgerTxSpec = utxoTxSpec
