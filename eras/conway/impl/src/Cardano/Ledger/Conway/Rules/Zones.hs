{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Zones where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  mkTxIx,
 )
import Cardano.Ledger.Conway.Core (
  Era (EraCrypto),
  EraRule,
  EraTx (Tx),
  InjectRuleFailure (..),
 )
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayZONES)
import Cardano.Ledger.Shelley.API (
  ShelleyLedgersEnv (LedgersEnv),
 )
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Sequence (Seq)
import GHC.Generics (Generic)

import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Conway.Core (EraGov)
import Cardano.Ledger.Core (EraIndependentTxBody, EraRuleFailure)
import Cardano.Ledger.Era (EraSegWits)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure,
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (txIxFromIntegral, txIxToInt)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Ledgers (ConwayLedgersEnv (ConwayLedgersEnv))
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Conway.Rules.Zone (ConwayZonePredFailure)
import Cardano.Ledger.Core (EraRuleEvent)
import Cardano.Ledger.Shelley.API (TxIx)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Control.Monad (foldM)
import qualified Data.Foldable as Foldale
import Data.Maybe (fromJust)
import NoThunks.Class (NoThunks)

data ConwayZonesPredFailure era
  = ZoneFailure (PredicateFailure (EraRule "ZONE" era)) -- Subtransition Failures
  | -- | ShelleyInConwayPredFailure (ShelleyLedgersPredFailure era) -- Subtransition Failures
    ZonesShelleyInConwayPredFailure (ShelleyBbodyPredFailure era) -- Subtransition Failures
  deriving (Generic)

data ConwayZonesEvent era
  = ZoneEvent (Event (EraRule "ZONE" era))
  | ZonesShelleyInConwayEvent (ShelleyLedgersEvent era)

type instance EraRuleFailure "ZONES" (ConwayEra c) = ConwayZonesPredFailure (ConwayEra c)

instance InjectRuleFailure "ZONES" ConwayZonesPredFailure (ConwayEra c)

type instance EraRuleFailure "ZONES" (ConwayEra c) = ConwayZonesPredFailure (ConwayEra c)

type instance EraRuleEvent "ZONES" (ConwayEra c) = ConwayZonesEvent (ConwayEra c)

instance InjectRuleFailure "ZONES" ConwayZonePredFailure (ConwayEra c) where
  injectFailure = ZoneFailure

instance InjectRuleFailure "ZONES" ShelleyLedgersPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayCertsPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" ConwayGovPredFailure (ConwayEra c) where
  injectFailure = ZoneFailure . injectFailure

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "ZONE" era))
  ) =>
  Show (ConwayZonesPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Eq (PredicateFailure (EraRule "ZONE" era))
  ) =>
  Eq (ConwayZonesPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  , NoThunks (PredicateFailure (EraRule "ZONE" era))
  ) =>
  NoThunks (ConwayZonesPredFailure era)

instance
  ( EraSegWits era
  , EraGov era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "ZONE" era) (ConwayZONES era)
  , Environment (EraRule "ZONE" era) ~ ConwayLedgersEnv era
  , State (EraRule "ZONE" era) ~ LedgerState era
  , Signal (EraRule "ZONE" era) ~ Seq (Tx era)
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  STS (ConwayZONES era)
  where
  type State (ConwayZONES era) = LedgerState era
  type Environment (ConwayZONES era) = ShelleyLedgersEnv era

  type Signal (ConwayZONES era) = Seq (Seq (Tx era))

  type BaseM (ConwayZONES era) = ShelleyBase

  type PredicateFailure (ConwayZONES era) = ConwayZonesPredFailure era

  type Event (ConwayZONES era) = ConwayZonesEvent era

  transitionRules = [zonesTransition]

-- Need to index each transaction in the list of lists by its index in the flattened list
-- Do we care about
zonesTransition ::
  forall era.
  ( Embed (EraRule "ZONE" era) (ConwayZONES era)
  , Environment (EraRule "ZONE" era) ~ ConwayLedgersEnv era
  , State (EraRule "ZONE" era) ~ LedgerState era
  , Signal (EraRule "ZONE" era) ~ Seq (Tx era)
  ) =>
  TransitionRule (ConwayZONES era)
zonesTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let indexedList = indexLists $ Foldale.toList (txwits :: Seq (Seq (Tx era)))

  case indexedList of
    Nothing -> undefined -- fail
    Just indexedTxList -> do
      foldM
        ( \ !ls' (ix, tx) ->
            trans @(EraRule "ZONE" era) $
              TRC (ConwayLedgersEnv slot ix pp account, ls', tx)
        )
        ls
        indexedTxList

indexLists :: [Seq a] -> Maybe [(TxIx, Seq a)]
indexLists = go (mkTxIx 0)
  where
    go :: TxIx -> [Seq a] -> Maybe [(TxIx, Seq a)]
    go _ [] = Just []
    go n (x : xs) = ((n, x) :) <$> next
      where
        mbIx = txIxFromIntegral $ length x
        -- This is technically partial, but only because the type inside TxIx is wrong (Word64 instead of Word16)
        next = (\n' -> go (fromJust $ txIxFromIntegral $ txIxToInt n + txIxToInt n') xs) =<< mbIx

instance
  forall era zone.
  ( Era era
  , BaseM zone ~ ShelleyBase
  , zone ~ EraRule "ZONE" era
  , STS zone
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Era era
  ) =>
  Embed zone (ConwayZONES era)
  where
  wrapFailed = ZoneFailure
  wrapEvent = ZoneEvent