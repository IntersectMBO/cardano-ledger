{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Bbody () where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.Rules.Ledgers ()
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPredFailure)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley

type instance EraRuleFailure "BBODY" BabbageEra = Alonzo.AlonzoBbodyPredFailure BabbageEra

instance InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure BabbageEra

instance InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgersPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgerPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxowPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxowPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxoPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxosPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPpupPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxoPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Allegra.AllegraUtxoPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelegsPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelplPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPoolPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyDelegPredFailure BabbageEra where
  injectFailure = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure . injectFailure
