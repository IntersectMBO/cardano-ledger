{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraBBODY,
  DijkstraCERT,
  DijkstraGOV,
  DijkstraGOVCERT,
  DijkstraLEDGER,
  DijkstraMEMPOOL,
  DijkstraSUBLEDGERS,
  DijkstraUTXO,
  DijkstraUTXOW,
) where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Internal.Era (DijkstraEra)
import Cardano.Ledger.Mary (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules

instance EraTxLevel DijkstraEra where
  type STxLevel l DijkstraEra = STxBothLevels l DijkstraEra

-------------------------------------------------------------------------------
-- Deprecated rules
-------------------------------------------------------------------------------

type instance EraRule "UPEC" DijkstraEra = VoidEraRule "UPEC" DijkstraEra

type instance EraRuleFailure "UPEC" DijkstraEra = VoidEraRule "UPEC" DijkstraEra

type instance EraRuleEvent "UPEC" DijkstraEra = VoidEraRule "UPEC" DijkstraEra

type instance EraRule "NEWPP" DijkstraEra = VoidEraRule "NEWPP" DijkstraEra

type instance EraRuleFailure "NEWPP" DijkstraEra = VoidEraRule "NEWPP" DijkstraEra

type instance EraRuleEvent "NEWPP" DijkstraEra = VoidEraRule "NEWPP" DijkstraEra

type instance EraRule "PPUP" DijkstraEra = VoidEraRule "PPUP" DijkstraEra

type instance EraRuleFailure "PPUP" DijkstraEra = VoidEraRule "PPUP" DijkstraEra

type instance EraRuleEvent "PPUP" DijkstraEra = VoidEraRule "PPUP" DijkstraEra

type instance EraRule "MIR" DijkstraEra = VoidEraRule "MIR" DijkstraEra

type instance EraRuleFailure "MIR" DijkstraEra = VoidEraRule "MIR" DijkstraEra

type instance EraRuleEvent "MIR" DijkstraEra = VoidEraRule "MIR" DijkstraEra

type instance EraRule "DELEGS" DijkstraEra = VoidEraRule "DELEGS" DijkstraEra

type instance EraRuleFailure "DELEGS" DijkstraEra = VoidEraRule "DELEGS" DijkstraEra

type instance EraRuleEvent "DELEGS" DijkstraEra = VoidEraRule "DELEGS" DijkstraEra

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

type instance Value DijkstraEra = MaryValue

data DijkstraSUBLEDGERS era

type instance EraRule "SUBLEDGERS" DijkstraEra = DijkstraSUBLEDGERS DijkstraEra

data DijkstraGOV era

type instance EraRule "GOV" DijkstraEra = DijkstraGOV DijkstraEra

type instance EraRule "NEWEPOCH" DijkstraEra = ConwayNEWEPOCH DijkstraEra

type instance EraRule "EPOCH" DijkstraEra = ConwayEPOCH DijkstraEra

type instance EraRule "ENACT" DijkstraEra = ConwayENACT DijkstraEra

type instance EraRule "UTXOS" DijkstraEra = ConwayUTXOS DijkstraEra

data DijkstraLEDGER era

type instance EraRule "LEDGER" DijkstraEra = DijkstraLEDGER DijkstraEra

type instance EraRule "TICKF" DijkstraEra = ConwayTICKF DijkstraEra

type instance EraRule "RATIFY" DijkstraEra = ConwayRATIFY DijkstraEra

type instance EraRule "CERTS" DijkstraEra = ConwayCERTS DijkstraEra

data DijkstraCERT era

type instance EraRule "CERT" DijkstraEra = DijkstraCERT DijkstraEra

type instance EraRule "DELEG" DijkstraEra = ConwayDELEG DijkstraEra

data DijkstraGOVCERT era

type instance EraRule "GOVCERT" DijkstraEra = DijkstraGOVCERT DijkstraEra

data DijkstraUTXOW era

type instance EraRule "UTXOW" DijkstraEra = DijkstraUTXOW DijkstraEra

data DijkstraUTXO era

type instance EraRule "UTXO" DijkstraEra = DijkstraUTXO DijkstraEra

data DijkstraBBODY era

type instance EraRule "BBODY" DijkstraEra = DijkstraBBODY DijkstraEra

data DijkstraMEMPOOL era

type instance EraRule "MEMPOOL" DijkstraEra = DijkstraMEMPOOL DijkstraEra

type instance EraRule "HARDFORK" DijkstraEra = ConwayHARDFORK DijkstraEra

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" DijkstraEra = API.ShelleyLEDGERS DijkstraEra

type instance EraRule "POOLREAP" DijkstraEra = API.ShelleyPOOLREAP DijkstraEra

type instance EraRule "RUPD" DijkstraEra = ShelleyRUPD DijkstraEra

type instance EraRule "SNAP" DijkstraEra = ShelleySNAP DijkstraEra

type instance EraRule "TICK" DijkstraEra = ShelleyTICK DijkstraEra

type instance EraRule "POOL" DijkstraEra = ShelleyPOOL DijkstraEra
