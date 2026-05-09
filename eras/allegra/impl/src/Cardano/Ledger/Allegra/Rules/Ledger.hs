{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Ledger () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Delegs ()
import Cardano.Ledger.Allegra.Rules.Utxow ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "LEDGER" AllegraEra = Shelley.ShelleyLedgerPredFailure AllegraEra

instance InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure AllegraEra

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxowPredFailure AllegraEra where
  injectFailure = Shelley.UtxowFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxoPredFailure AllegraEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPpupPredFailure AllegraEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegsPredFailure AllegraEra where
  injectFailure = Shelley.DelegsFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelplPredFailure AllegraEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPoolPredFailure AllegraEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegPredFailure AllegraEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

type instance EraRuleEvent "LEDGER" AllegraEra = Shelley.ShelleyLedgerEvent AllegraEra
