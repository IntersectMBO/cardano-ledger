{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Ledger () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Delegs ()
import Cardano.Ledger.Allegra.Rules.Utxow ()
import Cardano.Ledger.Core
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

type instance EraRuleFailure "LEDGER" AllegraEra = ShelleyLedgerPredFailure AllegraEra

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure AllegraEra

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure AllegraEra where
  injectFailure = UtxowFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure AllegraEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPpupPredFailure AllegraEra where
  injectFailure = UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure AllegraEra where
  injectFailure = DelegsFailure

instance InjectRuleFailure "LEDGER" ShelleyDelplPredFailure AllegraEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure AllegraEra where
  injectFailure = DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure AllegraEra where
  injectFailure = DelegsFailure . injectFailure

type instance EraRuleEvent "LEDGER" AllegraEra = ShelleyLedgerEvent AllegraEra
