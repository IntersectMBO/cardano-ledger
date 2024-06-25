{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Babel.Era (
  BabelBBODY,
  BabelEra,
  BabelLEDGERS,
  BabelUTXO,
  BabelUTXOS,
  BabelUTXOW,
  BabelLEDGER,
  BabelZONE,
  BabelZONES,
) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules (
  ConwayCERT,
  ConwayCERTS,
  ConwayDELEG,
  ConwayENACT,
  ConwayEPOCH,
  ConwayGOV,
  ConwayGOVCERT,
  ConwayNEWEPOCH,
  ConwayRATIFY,
  ConwayTICKF,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules (
  ShelleyPOOL,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
 )

-- =====================================================

-- | The Babel era
data BabelEra c

instance Crypto c => Era (BabelEra c) where
  type PreviousEra (BabelEra c) = ConwayEra c
  type EraCrypto (BabelEra c) = c
  type ProtVerLow (BabelEra c) = 9
  type ProtVerHigh (BabelEra c) = 10

  eraName = "Babel"

type instance Value (BabelEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Deprecated rules
-------------------------------------------------------------------------------

type instance EraRule "UPEC" (BabelEra c) = VoidEraRule "UPEC" (BabelEra c)
type instance EraRuleFailure "UPEC" (BabelEra c) = VoidEraRule "UPEC" (BabelEra c)
type instance EraRuleEvent "UPEC" (BabelEra c) = VoidEraRule "UPEC" (BabelEra c)

type instance EraRule "NEWPP" (BabelEra c) = VoidEraRule "NEWPP" (BabelEra c)
type instance EraRuleFailure "NEWPP" (BabelEra c) = VoidEraRule "NEWPP" (BabelEra c)
type instance EraRuleEvent "NEWPP" (BabelEra c) = VoidEraRule "NEWPP" (BabelEra c)

type instance EraRule "PPUP" (BabelEra c) = VoidEraRule "PPUP" (BabelEra c)
type instance EraRuleFailure "PPUP" (BabelEra c) = VoidEraRule "PPUP" (BabelEra c)
type instance EraRuleEvent "PPUP" (BabelEra c) = VoidEraRule "PPUP" (BabelEra c)

type instance EraRule "MIR" (BabelEra c) = VoidEraRule "MIR" (BabelEra c)
type instance EraRuleFailure "MIR" (BabelEra c) = VoidEraRule "MIR" (BabelEra c)
type instance EraRuleEvent "MIR" (BabelEra c) = VoidEraRule "MIR" (BabelEra c)

type instance EraRule "DELEGS" (BabelEra c) = VoidEraRule "DELEGS" (BabelEra c)
type instance EraRuleFailure "DELEGS" (BabelEra c) = VoidEraRule "DELEGS" (BabelEra c)
type instance EraRuleEvent "DELEGS" (BabelEra c) = VoidEraRule "DELEGS" (BabelEra c)

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data BabelUTXOS era

type instance EraRule "UTXOS" (BabelEra c) = BabelUTXOS (BabelEra c)

data BabelZONES era

type instance EraRule "ZONES" (BabelEra c) = BabelZONES (BabelEra c)

data BabelZONE era

type instance EraRule "ZONE" (BabelEra c) = BabelZONE (BabelEra c)

data BabelLEDGER era

type instance EraRule "LEDGER" (BabelEra c) = BabelLEDGER (BabelEra c)

data BabelLEDGERS era

type instance EraRule "LEDGERS" (BabelEra c) = BabelLEDGERS (BabelEra c)

data BabelUTXOW era

type instance EraRule "UTXOW" (BabelEra c) = BabelUTXOW (BabelEra c)

data BabelUTXO era

type instance EraRule "UTXO" (BabelEra c) = BabelUTXO (BabelEra c)

data BabelBBODY era

type instance EraRule "BBODY" (BabelEra c) = BabelBBODY (BabelEra c)

-- Rules inherited from Shelley

type instance EraRule "POOLREAP" (BabelEra c) = API.ShelleyPOOLREAP (BabelEra c)

type instance EraRule "RUPD" (BabelEra c) = ShelleyRUPD (BabelEra c)

type instance EraRule "SNAP" (BabelEra c) = ShelleySNAP (BabelEra c)

type instance EraRule "TICK" (BabelEra c) = ShelleyTICK (BabelEra c)

type instance EraRule "POOL" (BabelEra c) = ShelleyPOOL (BabelEra c)

-- Rules inherited from Conway

type instance EraRule "RATIFY" (BabelEra c) = ConwayRATIFY (BabelEra c)

type instance EraRule "ENACT" (BabelEra c) = ConwayENACT (BabelEra c)

type instance EraRule "GOV" (BabelEra c) = ConwayGOV (BabelEra c)

type instance EraRule "GOVCERT" (BabelEra c) = ConwayGOVCERT (BabelEra c)

type instance EraRule "TICKF" (BabelEra c) = ConwayTICKF (BabelEra c)

type instance EraRule "NEWEPOCH" (BabelEra c) = ConwayNEWEPOCH (BabelEra c)

type instance EraRule "EPOCH" (BabelEra c) = ConwayEPOCH (BabelEra c)

type instance EraRule "CERTS" (BabelEra c) = ConwayCERTS (BabelEra c)

type instance EraRule "CERT" (BabelEra c) = ConwayCERT (BabelEra c)

type instance EraRule "DELEG" (BabelEra c) = ConwayDELEG (BabelEra c)

-- =================================================
