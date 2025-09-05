{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.RoundTrip () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra (..))
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()

instance RuleListEra DijkstraEra where
  type
    EraRules DijkstraEra =
      '[ "BBODY"
       , "CERT"
       , "CERTS"
       , "DELEG"
       , "GOVCERT"
       , "GOV"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "UTXO"
       , "UTXOS"
       , "UTXOW"
       ]
