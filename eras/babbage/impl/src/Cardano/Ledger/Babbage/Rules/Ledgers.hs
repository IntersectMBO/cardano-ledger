{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Ledgers () where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Ledger ()
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPredFailure)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "LEDGERS" BabbageEra = Shelley.ShelleyLedgersPredFailure BabbageEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgersPredFailure BabbageEra

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyLedgerPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPpupPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegsPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelplPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyDelegPredFailure BabbageEra where
  injectFailure = Shelley.LedgerFailure . injectFailure
