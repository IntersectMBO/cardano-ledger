{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Ledger () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Delegs ()
import Cardano.Ledger.Mary.Rules.Utxow ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure,
  ShelleyLedgerEvent,
  ShelleyLedgerPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGER" MaryEra = ShelleyLedgerPredFailure MaryEra

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure MaryEra

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure MaryEra where
  injectFailure = UtxowFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure MaryEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPpupPredFailure MaryEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure MaryEra where
  injectFailure = DelegsFailure

instance InjectRuleFailure "LEDGER" ShelleyDelplPredFailure MaryEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure MaryEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure MaryEra where
  injectFailure = DelegsFailure . injectFailure

type instance EraRuleEvent "LEDGER" MaryEra = ShelleyLedgerEvent MaryEra
