-- | Defines the requirements on an era to be testable
module Test.Cardano.Ledger.TestableEra where

import Shelley.Spec.Ledger.API

class
  ( ApplyBlock era,
    ApplyTx era,
    GetLedgerView era
  ) =>
  TestableEra era
