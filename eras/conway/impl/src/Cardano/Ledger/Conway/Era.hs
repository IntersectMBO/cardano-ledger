{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Era (
  ConwayEra,
  ConwayBBODY,
  ConwayCERT,
  ConwayDELEG,
  ConwayGOVCERT,
  ConwayCERTS,
  ConwayGOV,
  ConwayHARDFORK,
  ConwayMEMPOOL,
  ConwayNEWEPOCH,
  ConwayEPOCH,
  ConwayENACT,
  ConwayUTXO,
  ConwayUTXOS,
  ConwayUTXOW,
  ConwayTICKF,
  ConwayLEDGER,
  ConwayRATIFY,
) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules (
  ShelleyPOOL,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
 )

-- =====================================================

-- | The Conway era
data ConwayEra

instance Era ConwayEra where
  type PreviousEra ConwayEra = BabbageEra
  type ProtVerLow ConwayEra = 9
  type ProtVerHigh ConwayEra = 10

  eraName = "Conway"

type instance Value ConwayEra = MaryValue

-------------------------------------------------------------------------------
-- Deprecated rules
-------------------------------------------------------------------------------

type instance EraRule "UPEC" ConwayEra = VoidEraRule "UPEC" ConwayEra
type instance EraRuleFailure "UPEC" ConwayEra = VoidEraRule "UPEC" ConwayEra
type instance EraRuleEvent "UPEC" ConwayEra = VoidEraRule "UPEC" ConwayEra

type instance EraRule "NEWPP" ConwayEra = VoidEraRule "NEWPP" ConwayEra
type instance EraRuleFailure "NEWPP" ConwayEra = VoidEraRule "NEWPP" ConwayEra
type instance EraRuleEvent "NEWPP" ConwayEra = VoidEraRule "NEWPP" ConwayEra

type instance EraRule "PPUP" ConwayEra = VoidEraRule "PPUP" ConwayEra
type instance EraRuleFailure "PPUP" ConwayEra = VoidEraRule "PPUP" ConwayEra
type instance EraRuleEvent "PPUP" ConwayEra = VoidEraRule "PPUP" ConwayEra

type instance EraRule "MIR" ConwayEra = VoidEraRule "MIR" ConwayEra
type instance EraRuleFailure "MIR" ConwayEra = VoidEraRule "MIR" ConwayEra
type instance EraRuleEvent "MIR" ConwayEra = VoidEraRule "MIR" ConwayEra

type instance EraRule "DELEGS" ConwayEra = VoidEraRule "DELEGS" ConwayEra
type instance EraRuleFailure "DELEGS" ConwayEra = VoidEraRule "DELEGS" ConwayEra
type instance EraRuleEvent "DELEGS" ConwayEra = VoidEraRule "DELEGS" ConwayEra

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data ConwayGOV era

type instance EraRule "GOV" ConwayEra = ConwayGOV ConwayEra

data ConwayNEWEPOCH era

type instance EraRule "NEWEPOCH" ConwayEra = ConwayNEWEPOCH ConwayEra

data ConwayEPOCH era

type instance EraRule "EPOCH" ConwayEra = ConwayEPOCH ConwayEra

data ConwayENACT era

type instance EraRule "ENACT" ConwayEra = ConwayENACT ConwayEra

data ConwayUTXOS era

type instance EraRule "UTXOS" ConwayEra = ConwayUTXOS ConwayEra

data ConwayLEDGER era

type instance EraRule "LEDGER" ConwayEra = ConwayLEDGER ConwayEra

data ConwayTICKF era

type instance EraRule "TICKF" ConwayEra = ConwayTICKF ConwayEra

data ConwayRATIFY era

type instance EraRule "RATIFY" ConwayEra = ConwayRATIFY ConwayEra

data ConwayCERTS era

type instance EraRule "CERTS" ConwayEra = ConwayCERTS ConwayEra

data ConwayCERT era

type instance EraRule "CERT" ConwayEra = ConwayCERT ConwayEra

data ConwayDELEG era

type instance EraRule "DELEG" ConwayEra = ConwayDELEG ConwayEra

data ConwayGOVCERT era

type instance EraRule "GOVCERT" ConwayEra = ConwayGOVCERT ConwayEra

data ConwayUTXOW era

type instance EraRule "UTXOW" ConwayEra = ConwayUTXOW ConwayEra

data ConwayUTXO era

type instance EraRule "UTXO" ConwayEra = ConwayUTXO ConwayEra

data ConwayBBODY era

type instance EraRule "BBODY" ConwayEra = ConwayBBODY ConwayEra

data ConwayMEMPOOL era

type instance EraRule "MEMPOOL" ConwayEra = ConwayMEMPOOL ConwayEra

data ConwayHARDFORK era

type instance EraRule "HARDFORK" ConwayEra = ConwayHARDFORK ConwayEra

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" ConwayEra = API.ShelleyLEDGERS ConwayEra

type instance EraRule "POOLREAP" ConwayEra = API.ShelleyPOOLREAP ConwayEra

type instance EraRule "RUPD" ConwayEra = ShelleyRUPD ConwayEra

type instance EraRule "SNAP" ConwayEra = ShelleySNAP ConwayEra

type instance EraRule "TICK" ConwayEra = ShelleyTICK ConwayEra

type instance EraRule "POOL" ConwayEra = ShelleyPOOL ConwayEra

-- =================================================
