{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Ledgers () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Ledger ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "LEDGERS" AllegraEra = Shelley.ShelleyLedgersPredFailure AllegraEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure AllegraEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgerPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPpupPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegsPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelplPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegPredFailure AllegraEra where
  injectFailure = Shelley.LedgerFailure . injectFailure
