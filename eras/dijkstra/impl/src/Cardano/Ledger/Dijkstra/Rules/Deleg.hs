{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Deleg () where

import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "DELEG" DijkstraEra = ConwayDelegPredFailure DijkstraEra

type instance EraRuleEvent "DELEG" DijkstraEra = VoidEraRule "DELEG" DijkstraEra

instance InjectRuleFailure "DELEG" ConwayDelegPredFailure DijkstraEra

instance InjectRuleFailure "DELEG" Shelley.AccountAlreadyRegistered DijkstraEra where
  injectFailure = Conway.DelegAccountAlreadyRegistered
