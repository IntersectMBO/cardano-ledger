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

module Cardano.Ledger.Dijkstra.Rules.SubUtxow (
  DijkstraSUBUTXOW,
  DijkstraSubUtxowPredFailure (..),
  DijkstraSubUtxowEvent (..),
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
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.SubUtxo (
  DijkstraSubUtxoPredFailure,
 )
import Cardano.Ledger.Shelley.LedgerState (UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

newtype DijkstraSubUtxowPredFailure era
  = SubUtxoFailure (PredicateFailure (EraRule "SUBUTXO" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBUTXO" era)) => Eq (DijkstraSubUtxowPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBUTXO" era)) => Show (DijkstraSubUtxowPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "SUBUTXO" era)) =>
  NoThunks (DijkstraSubUtxowPredFailure era)

instance
  NFData (PredicateFailure (EraRule "SUBUTXO" era)) =>
  NFData (DijkstraSubUtxowPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  EncCBOR (DijkstraSubUtxowPredFailure era)
  where
  encCBOR (SubUtxoFailure e) = encCBOR e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBUTXO" era))
  ) =>
  DecCBOR (DijkstraSubUtxowPredFailure era)
  where
  decCBOR = SubUtxoFailure <$> decCBOR

type instance EraRuleFailure "SUBUTXOW" DijkstraEra = DijkstraSubUtxowPredFailure DijkstraEra

type instance EraRuleEvent "SUBUTXOW" DijkstraEra = DijkstraSubUtxowEvent DijkstraEra

instance InjectRuleFailure "SUBUTXOW" DijkstraSubUtxowPredFailure DijkstraEra

instance InjectRuleFailure "SUBUTXOW" DijkstraSubUtxoPredFailure DijkstraEra where
  injectFailure = SubUtxoFailure

instance InjectRuleEvent "SUBUTXOW" DijkstraSubUtxowEvent DijkstraEra

newtype DijkstraSubUtxowEvent era = SubUtxo (Event (EraRule "SUBUTXO" era))
  deriving (Generic)

deriving instance Eq (Event (EraRule "SUBUTXO" era)) => Eq (DijkstraSubUtxowEvent era)

instance NFData (Event (EraRule "SUBUTXO" era)) => NFData (DijkstraSubUtxowEvent era)

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , Embed (EraRule "SUBUTXO" era) (DijkstraSUBUTXOW era)
  ) =>
  STS (DijkstraSUBUTXOW era)
  where
  type State (DijkstraSUBUTXOW era) = UTxOState era
  type Signal (DijkstraSUBUTXOW era) = Tx SubTx era
  type Environment (DijkstraSUBUTXOW era) = UtxoEnv era
  type BaseM (DijkstraSUBUTXOW era) = ShelleyBase
  type PredicateFailure (DijkstraSUBUTXOW era) = DijkstraSubUtxowPredFailure era
  type Event (DijkstraSUBUTXOW era) = DijkstraSubUtxowEvent era

  transitionRules = [dijkstraSubUtxowTransition @era]

dijkstraSubUtxowTransition ::
  forall era.
  ( EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , Embed (EraRule "SUBUTXO" era) (DijkstraSUBUTXOW era)
  ) =>
  TransitionRule (EraRule "SUBUTXOW" era)
dijkstraSubUtxowTransition = do
  TRC (env, state, signal) <- judgmentContext
  trans @(EraRule "SUBUTXO" era) $ TRC (env, state, signal)

instance
  ( ConwayEraGov era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  ) =>
  Embed (DijkstraSUBUTXO era) (DijkstraSUBUTXOW era)
  where
  wrapFailed = SubUtxoFailure
  wrapEvent = SubUtxo
