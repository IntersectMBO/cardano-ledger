{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Utxo () where

import Cardano.Ledger.Allegra.Rules (shelleyToAllegraUtxoPredFailure)
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoEvent, AlonzoUtxoPredFailure, AlonzoUtxosPredFailure)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Conway.Rules (
  ConwayUtxoPredFailure (..),
  ConwayUtxosPredFailure,
  allegraToConwayUtxoPredFailure,
  alonzoToConwayUtxoPredFailure,
  babbageToConwayUtxoPredFailure,
 )
import Cardano.Ledger.Dijkstra.Core (EraRuleEvent, EraRuleFailure, InjectRuleFailure (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Utxos ()
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure)

type instance EraRuleFailure "UTXO" DijkstraEra = ConwayUtxoPredFailure DijkstraEra

type instance EraRuleEvent "UTXO" DijkstraEra = AlonzoUtxoEvent DijkstraEra

instance InjectRuleFailure "UTXO" ConwayUtxoPredFailure DijkstraEra

instance InjectRuleFailure "UTXO" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = babbageToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = alonzoToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    allegraToConwayUtxoPredFailure
      . shelleyToAllegraUtxoPredFailure

instance InjectRuleFailure "UTXO" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = allegraToConwayUtxoPredFailure

instance InjectRuleFailure "UTXO" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = UtxosFailure

instance InjectRuleFailure "UTXO" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    alonzoToConwayUtxoPredFailure
      . Alonzo.UtxosFailure
      . injectFailure
