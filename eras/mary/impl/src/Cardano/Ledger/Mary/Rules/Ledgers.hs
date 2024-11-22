{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Ledgers () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Ledger ()
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyDelegsPredFailure,
  ShelleyDelplPredFailure,
  ShelleyLedgerPredFailure,
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyPpupPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

type instance EraRuleFailure "LEDGERS" MaryEra = ShelleyLedgersPredFailure MaryEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure MaryEra

instance InjectRuleFailure "LEDGERS" ShelleyLedgerPredFailure MaryEra where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPpupPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegsPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelplPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyDelegPredFailure MaryEra where
  injectFailure = LedgerFailure . injectFailure
