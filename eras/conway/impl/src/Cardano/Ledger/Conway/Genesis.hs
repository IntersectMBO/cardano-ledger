module Cardano.Ledger.Conway.Genesis
  ( ConwayGenesis (..),
    extendPPWithGenesis,
  )
where

import Cardano.Ledger.Babbage.Genesis (extendPPWithGenesis)
import Cardano.Ledger.Keys (GenDelegs)

newtype ConwayGenesis crypto = ConwayGenesis (GenDelegs crypto)
