{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Ledgers () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Ledger ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "LEDGERS" MaryEra = Shelley.ShelleyLedgersPredFailure MaryEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure MaryEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgerPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPpupPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegsPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelplPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegPredFailure MaryEra where
  injectFailure = Shelley.LedgerFailure . injectFailure
