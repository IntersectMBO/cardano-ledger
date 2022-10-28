{-# OPTIONS_GHC -Wno-dodgy-exports #-}

-- This "dodgy" option is needed because Cardano.Ledger.Orphans exports nothing
-- but makes a bunch of orphan instances, and it is these orphan instances we
-- want to be exported. The dodgy-exports Flag does not seem to understand about instances.

module Cardano.Ledger.Shelley.Orphans
  {-# DEPRECATED "Use 'import Cardano.Ledger.Orphans' instead" #-}
  ( module X,
  )
where

import Cardano.Ledger.Orphans as X ()
