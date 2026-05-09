{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Bbody () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Ledgers ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "BBODY" AllegraEra = Shelley.ShelleyBbodyPredFailure AllegraEra

instance InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure AllegraEra

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgersPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgerPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxowPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxoPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPpupPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelegsPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelplPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPoolPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelegPredFailure AllegraEra where
  injectFailure = Shelley.LedgersFailure . injectFailure
