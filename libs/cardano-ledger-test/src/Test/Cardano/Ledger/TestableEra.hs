-- | Defines the requirements on an era to be testable
module Test.Cardano.Ledger.TestableEra where

import Cardano.Ledger.Shelley.API

class
  ( ApplyBlock era,
    ApplyTx era,
    GetLedgerView era
  ) =>
  TestableEra era
