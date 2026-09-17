{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Rules.Bbody () where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Core
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Babbage.Era (BBODY, BabbageEra)
import Cardano.Ledger.Babbage.Rules.Ledgers ()
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.Rules.Utxow (BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Block (PraosBbodySignal (..))
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Embed (..), STS (..), withJudgmentContext)
import Data.Sequence (Seq)

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

instance
  ( EraRule "BBODY" era ~ BBODY era
  , InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (BBODY era)
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , AlonzoEraTxWits era
  , EraBlockBody era
  , AlonzoEraPParams era
  , AlonzoEraTx era
  ) =>
  STS (BBODY era)
  where
  type State (BBODY era) = Shelley.ShelleyBbodyState era

  type Signal (BBODY era) = PraosBbodySignal era

  type Environment (BBODY era) = Shelley.BbodyEnv era

  type BaseM (BBODY era) = ShelleyBase

  type PredicateFailure (BBODY era) = Alonzo.AlonzoBbodyPredFailure era
  type Event (BBODY era) = Alonzo.AlonzoBbodyEvent era

  initialRules = []
  transitionRules =
    [ withJudgmentContext $ \env state (PraosBbodySignal block) ->
        Alonzo.bbodyTransition env state block
    ]

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  , Era era
  ) =>
  Embed ledgers (BBODY era)
  where
  wrapFailed = Alonzo.ShelleyInAlonzoBbodyPredFailure . Shelley.LedgersFailure
  wrapEvent = Alonzo.ShelleyInAlonzoEvent . Shelley.LedgersEvent
