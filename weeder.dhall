{ roots =
  [ "^Cardano.Ledger.Shelley.API.*$"
  , "^Cardano.Chain.Byron.API.*$"
  , "^Main.main$"
  , "^Tests.main$"
    -- The following modules use some TH discovery, so we add them as explicit
    -- roots.
  , "^Test.Cardano.Chain.*$"
  , "^Test.Cardano.Crypto.*$"

  , "^Test.Cardano.Ledger.Shelley.*$"
    -- Called in ouroboros-node
  , "^Cardano.Ledger.Shelley.Genesis.*"
  , "^Cardano.Ledger.Shelley.TxData.TxOut$"
  , "^Cardano.Ledger.Shelley.Scripts.hashAnyScript$"

    -- Things we're not interested in
  , "^Test.Goblin.*"

    -- Testing stuff we want to keep
  , "Test.Byron.Spec.Ledger.UTxO.Properties.tracesAreClassified"
  ]
, type-class-roots = True
}
