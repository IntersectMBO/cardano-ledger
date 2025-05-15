{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
) where

import Cardano.Ledger.Conway.Core (Era (..), Value, EraRule)
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Conway.Rules
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Conway (ConwayEra)

data DijkstraEra

instance Era DijkstraEra where
  type PreviousEra DijkstraEra = ConwayEra
  type ProtVerLow DijkstraEra = 12
  type ProtVerHigh DijkstraEra = 12

  eraName = "Dijkstra"

type instance Value DijkstraEra = MaryValue

type instance EraRule "GOV" DijkstraEra = ConwayGOV DijkstraEra

type instance EraRule "NEWEPOCH" DijkstraEra = ConwayNEWEPOCH DijkstraEra

type instance EraRule "EPOCH" DijkstraEra = ConwayEPOCH DijkstraEra

type instance EraRule "ENACT" DijkstraEra = ConwayENACT DijkstraEra

type instance EraRule "UTXOS" DijkstraEra = ConwayUTXOS DijkstraEra

type instance EraRule "LEDGER" DijkstraEra = ConwayLEDGER DijkstraEra

type instance EraRule "TICKF" DijkstraEra = ConwayTICKF DijkstraEra

type instance EraRule "RATIFY" DijkstraEra = ConwayRATIFY DijkstraEra

type instance EraRule "CERTS" DijkstraEra = ConwayCERTS DijkstraEra

type instance EraRule "CERT" DijkstraEra = ConwayCERT DijkstraEra

type instance EraRule "DELEG" DijkstraEra = ConwayDELEG DijkstraEra

type instance EraRule "GOVCERT" DijkstraEra = ConwayGOVCERT DijkstraEra

type instance EraRule "UTXOW" DijkstraEra = ConwayUTXOW DijkstraEra

type instance EraRule "UTXO" DijkstraEra = ConwayUTXO DijkstraEra

type instance EraRule "BBODY" DijkstraEra = ConwayBBODY DijkstraEra

type instance EraRule "MEMPOOL" DijkstraEra = ConwayMEMPOOL DijkstraEra

type instance EraRule "HARDFORK" DijkstraEra = ConwayHARDFORK DijkstraEra

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" DijkstraEra = API.ShelleyLEDGERS DijkstraEra

type instance EraRule "POOLREAP" DijkstraEra = API.ShelleyPOOLREAP DijkstraEra

type instance EraRule "RUPD" DijkstraEra = ShelleyRUPD DijkstraEra

type instance EraRule "SNAP" DijkstraEra = ShelleySNAP DijkstraEra

type instance EraRule "TICK" DijkstraEra = ShelleyTICK DijkstraEra

type instance EraRule "POOL" DijkstraEra = ShelleyPOOL DijkstraEra
