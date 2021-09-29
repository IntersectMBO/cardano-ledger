module Cardano.Ledger.Shelley.Rules.Chain
  {-# DEPRECATED "Use 'import Test.Cardano.Ledger.Shelley.Rules.Chain' for the CHAIN rule, use 'import Cardano.Ledger.Chain' for chainChecks, and use 'import Cardano.Ledger.Shelley.API.Wallet' for AdaPots." #-}
  (module X)
where

import Cardano.Ledger.Chain as X
import Cardano.Ledger.Shelley.API.Wallet as X (AdaPots (..), totalAdaES, totalAdaPotsES)
