{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubUtxo (
  DijkstraSUBUTXO,
  DijkstraSubUtxoPredFailure (..),
  DijkstraSubUtxoEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxos (DijkstraSubUtxosPredFailure)
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubUtxoPredFailure era
  = SubUtxosFailure (PredicateFailure (EraRule "SUBUTXOS" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBUTXOS" era)) => Eq (DijkstraSubUtxoPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBUTXOS" era)) => Show (DijkstraSubUtxoPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "SUBUTXOS" era)) =>
  NoThunks (DijkstraSubUtxoPredFailure era)

instance
  NFData (PredicateFailure (EraRule "SUBUTXOS" era)) =>
  NFData (DijkstraSubUtxoPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBUTXOS" era))
  ) =>
  EncCBOR (DijkstraSubUtxoPredFailure era)
  where
  encCBOR (SubUtxosFailure e) = encCBOR e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBUTXOS" era))
  ) =>
  DecCBOR (DijkstraSubUtxoPredFailure era)
  where
  decCBOR = SubUtxosFailure <$> decCBOR

type instance EraRuleFailure "SUBUTXO" DijkstraEra = DijkstraSubUtxoPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXO" DijkstraEra = DijkstraSubUtxoEvent DijkstraEra

instance InjectRuleFailure "SUBUTXO" DijkstraSubUtxoPredFailure DijkstraEra

instance InjectRuleFailure "SUBUTXO" DijkstraSubUtxosPredFailure DijkstraEra where
  injectFailure = SubUtxosFailure

instance InjectRuleEvent "SUBUTXO" DijkstraSubUtxoEvent DijkstraEra

newtype DijkstraSubUtxoEvent era = SubUtxosEvent (Event (EraRule "SUBUTXOS" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBUTXOS" era)) => Eq (DijkstraSubUtxoEvent era)

instance NFData (Event (EraRule "SUBUTXOS" era)) => NFData (DijkstraSubUtxoEvent era)

instance
  ( ConwayEraGov era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , Embed (EraRule "SUBUTXOS" era) (DijkstraSUBUTXO era)
  ) =>
  STS (DijkstraSUBUTXO era)
  where
  type State (DijkstraSUBUTXO era) = UTxOState era
  type Signal (DijkstraSUBUTXO era) = Tx SubTx era
  type Environment (DijkstraSUBUTXO era) = UtxoEnv era
  type BaseM (DijkstraSUBUTXO era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXO era) = DijkstraSubUtxoPredFailure era
  type Event (DijkstraSUBUTXO era) = DijkstraSubUtxoEvent era

  transitionRules = [dijkstraSubUtxoTransition @era]

dijkstraSubUtxoTransition ::
  forall era.
  ( EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , Embed (EraRule "SUBUTXOS" era) (DijkstraSUBUTXO era)
  ) =>
  TransitionRule (EraRule "SUBUTXO" era)
dijkstraSubUtxoTransition = do
  TRC (env, state, signal) <- judgmentContext
  trans @(EraRule "SUBUTXOS" era) $
    TRC (env, state, signal)

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  ) =>
  Embed (DijkstraSUBUTXOS era) (DijkstraSUBUTXO era)
  where
  wrapFailed = SubUtxosFailure
  wrapEvent = SubUtxosEvent
