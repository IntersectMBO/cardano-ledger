{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Rules.Ledger () where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.Rules.Delegs ()
import Cardano.Ledger.Mary.Rules.Utxow ()
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "LEDGER" MaryEra = Shelley.ShelleyLedgerPredFailure MaryEra

instance InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure MaryEra

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxowPredFailure MaryEra where
  injectFailure = Shelley.UtxowFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxoPredFailure MaryEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPpupPredFailure MaryEra where
  injectFailure = Shelley.UtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegsPredFailure MaryEra where
  injectFailure = Shelley.DelegsFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelplPredFailure MaryEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPoolPredFailure MaryEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyDelegPredFailure MaryEra where
  injectFailure = Shelley.DelegsFailure . injectFailure

type instance EraRuleEvent "LEDGER" MaryEra = Shelley.ShelleyLedgerEvent MaryEra
