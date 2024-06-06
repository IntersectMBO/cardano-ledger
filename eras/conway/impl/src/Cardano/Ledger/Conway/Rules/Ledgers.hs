{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ledgers (ConwayLEDGERS, ConwayLedgersEnv (..)) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayLEDGERS)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLEDGER, ConwayLedgerEvent, ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.API.Types (AccountState, LedgerEnv (LedgerEnv), LedgerState)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEvent (LedgerEvent),
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.TxIn (TxIx)
import Control.Monad (foldM)
import Control.State.Transition (
  Embed (wrapEvent, wrapFailed),
  STS (..),
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Default.Class (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

data ConwayLedgersEnv era = ConwayLedgersEnv
  { ledgerSlotNo :: !SlotNo
  , ledgerIxStart :: !TxIx
  , ledgerPp :: !(PParams era)
  , ledgerAccount :: !AccountState
  }
  deriving (Generic)

type instance EraRuleFailure "LEDGERS" (ConwayEra c) = ShelleyLedgersPredFailure (ConwayEra c)

type instance EraRuleEvent "LEDGERS" (ConwayEra c) = ShelleyLedgersEvent (ConwayEra c)

instance InjectRuleFailure "LEDGERS" ShelleyLedgersPredFailure (ConwayEra c)

instance InjectRuleFailure "LEDGERS" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertsPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovPredFailure (ConwayEra c) where
  injectFailure = LedgerFailure . injectFailure

instance
  ( Era era
  , Embed (EraRule "LEDGER" era) (ConwayLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Default (LedgerState era)
  ) =>
  STS (ConwayLEDGERS era)
  where
  type State (ConwayLEDGERS era) = LedgerState era
  type Signal (ConwayLEDGERS era) = Seq (Tx era)
  type Environment (ConwayLEDGERS era) = ConwayLedgersEnv era
  type BaseM (ConwayLEDGERS era) = ShelleyBase
  type PredicateFailure (ConwayLEDGERS era) = ShelleyLedgersPredFailure era
  type Event (ConwayLEDGERS era) = ShelleyLedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( Embed (EraRule "LEDGER" era) (ConwayLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Signal (EraRule "LEDGER" era) ~ Tx era
  ) =>
  TransitionRule (ConwayLEDGERS era)
ledgersTransition = do
  TRC (ConwayLedgersEnv slot ixStart pp account, ls, txwits) <-
    judgmentContext
  foldM
    ( \ !ls' (ix, tx) ->
        trans @(EraRule "LEDGER" era) $
          TRC (LedgerEnv slot ix pp account, ls', tx)
    )
    ls
    $ zip [ixStart ..]
    $ toList txwits

instance
  ( Era era
  , STS (ConwayLEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  ) =>
  Embed (ConwayLEDGER era) (ConwayLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent