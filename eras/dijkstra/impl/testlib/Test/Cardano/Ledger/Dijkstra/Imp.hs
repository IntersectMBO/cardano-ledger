{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Imp where

import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraMempoolPredFailure, DijkstraUtxoPredFailure)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError)
import Cardano.Ledger.Shelley.Rules
import Data.List.NonEmpty (NonEmpty)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import qualified Test.Cardano.Ledger.Dijkstra.Imp.MempoolSpec as Mempool
import qualified Test.Cardano.Ledger.Dijkstra.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Dijkstra.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Dijkstra.ImpTest

spec ::
  forall era.
  ( DijkstraEraImp era
  , EraSpecificSpec era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure era
  , Inject (NonEmpty (DijkstraMempoolPredFailure era)) (ApplyTxError era)
  , PredicateFailure (EraRule "MEMPOOL" era) ~ DijkstraMempoolPredFailure era
  ) =>
  Spec
spec = do
  ConwayImp.spec @era
  withEachEraVersion @era $ dijkstraEraGenericSpec @era

dijkstraEraGenericSpec ::
  forall era.
  ( DijkstraEraImp era
  , InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure era
  , PredicateFailure (EraRule "MEMPOOL" era) ~ DijkstraMempoolPredFailure era
  , Inject (NonEmpty (DijkstraMempoolPredFailure era)) (ApplyTxError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
dijkstraEraGenericSpec = do
  describe "UTXOW" Utxow.spec
  describe "UTXO" Utxo.spec
  describe "MEMPOOL" Mempool.spec

instance EraSpecificSpec DijkstraEra
