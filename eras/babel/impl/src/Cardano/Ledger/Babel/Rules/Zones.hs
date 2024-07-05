{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Cardano.Ledger.Babel.Rules.Zones where

import Cardano.Ledger.Babel.Core (
  Era (EraCrypto),
  EraGov,
  EraRule,
  EraTx (Tx),
  InjectRuleFailure (..),
 )
import Cardano.Ledger.Babel.Era (BabelEra, BabelZONES)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
  mkTxIx,
  txIxFromIntegral,
  txIxToInt,
 )
import Cardano.Ledger.Shelley.API (
  ShelleyLedgersEnv (LedgersEnv),
  TxIx,
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
import Cardano.Ledger.Core (
  EraIndependentTxBody,
  EraRuleEvent,
  EraRuleFailure,
 )
import Cardano.Ledger.Era (EraSegWits)
import Cardano.Ledger.Keys (DSignable)
import Cardano.Ledger.Shelley.Rules (
  ShelleyBbodyPredFailure,
  ShelleyLedgersEvent,
  ShelleyLedgersPredFailure,
 )

import Cardano.Ledger.Babel.Rules.Ledger (BabelLedgerPredFailure)
import Cardano.Ledger.Babel.Rules.Ledgers (BabelLedgersEnv (BabelLedgersEnv))
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxos (BabelUtxosPredFailure)
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.Babel.Rules.Zone (BabelZonePredFailure)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Control.Monad (foldM)
import qualified Data.Foldable as Foldale
import Data.Maybe (fromJust)
import NoThunks.Class (NoThunks)

data BabelZonesPredFailure era
  = ZoneFailure (PredicateFailure (EraRule "ZONE" era)) -- Subtransition Failures
  | ZonesShelleyInBabelPredFailure (ShelleyBbodyPredFailure era) -- Subtransition Failures
  deriving (Generic)

data BabelZonesEvent era
  = ZoneEvent (Event (EraRule "ZONE" era))
  | ZonesShelleyInBabelEvent (ShelleyLedgersEvent era)

type instance EraRuleFailure "ZONES" (BabelEra c) = BabelZonesPredFailure (BabelEra c)

instance InjectRuleFailure "ZONES" BabelZonesPredFailure (BabelEra c)

type instance EraRuleFailure "ZONES" (BabelEra c) = BabelZonesPredFailure (BabelEra c)

type instance EraRuleEvent "ZONES" (BabelEra c) = BabelZonesEvent (BabelEra c)

instance InjectRuleFailure "ZONES" BabelZonePredFailure (BabelEra c) where
  injectFailure = ZoneFailure

instance InjectRuleFailure "ZONES" ShelleyLedgersPredFailure (BabelEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" BabelLedgerPredFailure (BabelEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = ZoneFailure . injectFailure

instance InjectRuleFailure "ZONES" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = ZoneFailure . injectFailure

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "ZONE" era))
  ) =>
  Show (BabelZonesPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Eq (PredicateFailure (EraRule "ZONE" era))
  ) =>
  Eq (BabelZonesPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  , NoThunks (PredicateFailure (EraRule "ZONE" era))
  ) =>
  NoThunks (BabelZonesPredFailure era)

instance
  ( EraSegWits era
  , EraGov era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "ZONE" era) (BabelZONES era)
  , Environment (EraRule "ZONE" era) ~ BabelLedgersEnv era
  , State (EraRule "ZONE" era) ~ LedgerState era
  , Signal (EraRule "ZONE" era) ~ Seq (Tx era)
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  STS (BabelZONES era)
  where
  type State (BabelZONES era) = LedgerState era
  type Environment (BabelZONES era) = ShelleyLedgersEnv era

  type Signal (BabelZONES era) = Seq (Seq (Tx era))

  type BaseM (BabelZONES era) = ShelleyBase

  type PredicateFailure (BabelZONES era) = BabelZonesPredFailure era

  type Event (BabelZONES era) = BabelZonesEvent era

  transitionRules = [zonesTransition]

{- CIP-0118#ZONES-rule

Previously, LEDGERS was indexing transactions by their position in the block.

Now, LEDGERS can only see transactions in a zone, and thus can only index transactions
relative to that zone.

To solve this, in ZONES, we index each zone by its position in the block. This
gives LEDGERS the knowledge of a "starting point" from which to derive the
absolute position of transactions in a block given its relative position in the zone.

Jump to CIP-0118#ZONE-rule to continue... -}

-- Need to index each transaction in the list of lists by its index in the flattened list
zonesTransition ::
  forall era.
  ( Embed (EraRule "ZONE" era) (BabelZONES era)
  , Environment (EraRule "ZONE" era) ~ BabelLedgersEnv era
  , State (EraRule "ZONE" era) ~ LedgerState era
  , Signal (EraRule "ZONE" era) ~ Seq (Tx era)
  ) =>
  TransitionRule (BabelZONES era)
zonesTransition = do
  TRC (LedgersEnv slot pp account, ls, txwits) <- judgmentContext
  let indexedList = indexLists $ Foldale.toList (txwits :: Seq (Seq (Tx era)))

  case indexedList of
    Nothing -> undefined -- fail
    Just indexedTxList ->
      foldM
        ( \ !ls' (ix, tx) ->
            trans @(EraRule "ZONE" era) $
              TRC (BabelLedgersEnv slot ix pp account, ls', tx)
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
  Embed zone (BabelZONES era)
  where
  wrapFailed = ZoneFailure
  wrapEvent = ZoneEvent