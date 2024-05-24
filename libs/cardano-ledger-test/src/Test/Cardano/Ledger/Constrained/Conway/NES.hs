{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Cardano.Ledger.Constrained.Conway.NES where

import Cardano.Ledger.Shelley.HardForks qualified as HardForks
import Data.Foldable

import Data.Coerce

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Data.Map qualified as Map
import Data.Set qualified as Set
import Lens.Micro

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Lens.Micro qualified as L
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Constrained.Conway.Gov

-- TODO: this spec probably produces terrible distributions of states!!
nesSpec :: IsConwayUniv ConwayFn
        => Specification ConwayFn (NewEpochState (ConwayEra StandardCrypto))
nesSpec = constrained $ \ nes ->
  [ cgsProposals_ (utxosGovState_ (lsUTxOState_ (esLState_ (nesEs_ nes)))) `satisfies` govProposalsSpec (lit $ EpochNo 0) cSNothing_
  ]

