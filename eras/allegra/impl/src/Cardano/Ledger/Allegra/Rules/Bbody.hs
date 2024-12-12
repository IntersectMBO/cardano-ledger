{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Rules.Bbody () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Rules.Ledgers ()
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure (..),
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure,
  ShelleyLedgerPredFailure,
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "BBODY" AllegraEra = ShelleyBbodyPredFailure AllegraEra

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure AllegraEra

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure AllegraEra where
  injectFailure = LedgersFailure

instance InjectRuleFailure "BBODY" ShelleyLedgerPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPpupPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegsPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelplPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegPredFailure AllegraEra where
  injectFailure = LedgersFailure . injectFailure
