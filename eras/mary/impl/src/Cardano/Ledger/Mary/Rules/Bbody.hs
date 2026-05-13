{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Bbody () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Ledgers ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "BBODY" MaryEra = Shelley.ShelleyBbodyPredFailure MaryEra

instance InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure MaryEra

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgersPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgerPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxowPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxoPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPpupPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelegsPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelplPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPoolPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelegPredFailure MaryEra where
  injectFailure = Shelley.LedgersFailure . injectFailure
