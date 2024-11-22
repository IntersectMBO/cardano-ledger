{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Bbody () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Ledgers ()
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

type instance EraRuleFailure "BBODY" MaryEra = ShelleyBbodyPredFailure MaryEra

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure MaryEra

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure MaryEra where
  injectFailure = LedgersFailure

instance InjectRuleFailure "BBODY" ShelleyLedgerPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPpupPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegsPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelplPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegPredFailure MaryEra where
  injectFailure = LedgersFailure . injectFailure
