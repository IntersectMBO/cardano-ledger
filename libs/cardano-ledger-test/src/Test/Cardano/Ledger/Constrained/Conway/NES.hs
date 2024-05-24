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
  [ nesEs_ nes `satisfies` esSpec (lit $ EpochNo 0) -- TODO: not the right epochNo
  ]

esSpecSTS :: IsConwayUniv ConwayFn
          => Specification ConwayFn (EpochState (ConwayEra StandardCrypto))
esSpecSTS = esSpec (lit $ EpochNo 0) -- TODO: this is not a good epochNo, probably it should be taken from the internal state but digging it out sucks!

esSpec :: IsConwayUniv ConwayFn
       => Term ConwayFn EpochNo
       -> Specification ConwayFn (EpochState (ConwayEra StandardCrypto))
esSpec epochNo = constrained $ \ es ->
  cgsProposals_ (utxosGovState_ (lsUTxOState_ (esLState_ es))) `satisfies` govProposalsSpec epochNo cSNothing_ -- TODO: could be something!
