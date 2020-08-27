{ roots =
  [ "^Shelley.Spec.Ledger.API.*$"
  , "^Cardano.Chain.Byron.API.*$"
  , "^Main.main$"
  , "^Tests.main$"
    -- The following modules use some TH discovery, so we add them as explicit
    -- roots.
  , "^Test.Cardano.Chain.*$"
  , "^Test.Cardano.Crypto.*$"

  , "^Test.Shelley.Spec.Ledger.*$"
    -- Called in ouroboros-node
  , "^Shelley.Spec.Ledger.Genesis.*"
  , "^Shelley.Spec.Ledger.TxData.TxOut$"
  , "^Shelley.Spec.Ledger.Scripts.hashAnyScript$"

    -- Things we're not interested in
  , "^Test.Goblin.*"

    -- Testing stuff we want to keep
  , "Test.Byron.Spec.Ledger.UTxO.Properties.tracesAreClassified"
  ]
, type-class-roots = True
}
