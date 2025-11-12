{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Dijkstra.Rules.Bbody (
  DijkstraBBODY,
  DijkstraBbodyPredFailure (..),
  alonzoToDijkstraBbodyPredFailure,
  shelleyToDijkstraBbodyPredFailure,
  conwayToDijkstraBbodyPredFailure,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyEvent(..),
  AlonzoBbodyPredFailure,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
  -- alonzoBbodyTransition,
 )
import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
 )
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure,
  ConwayCertPredFailure,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayLedgerPredFailure,
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure,
  ConwayUtxowPredFailure,
  alonzoToConwayBbodyPredFailure,
  -- conwayBbodyTransition,
  shelleyToConwayBbodyPredFailure,
 )
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraBlockBody)
import Cardano.Ledger.Dijkstra.Core (
  AlonzoEraPParams,
  AlonzoEraTx,
  AlonzoEraTxWits,
  BabbageEraTxBody,
  ConwayEraPParams,
  EraBlockBody (..),
  EraRule,
  EraRuleEvent,
  EraRuleFailure,
  EraTx (..),
  InjectRuleFailure (..),
  TxLevel (..), Era,
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraBBODY, DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Shelley.API (LedgerState, ShelleyLedgersEnv)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyPredFailure(..),
  ShelleyBbodyState (..),
  ShelleyBbodyEvent (..),
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Embed, STS (..), TRC (..), TransitionRule, judgmentContext)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import NoThunks.Class (NoThunks)
import Cardano.Ledger.Binary.Coders (Decode (..), decode, (<!))
import Control.State.Transition.Extended (Embed(..))

newtype DijkstraBbodyPredFailure era
  = DijkstraBbodyPredFailure (ConwayBbodyPredFailure era) -- Temporary wrapper
  deriving (Generic)

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGERS" era))) =>
  Show (DijkstraBbodyPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGERS" era))) =>
  Eq (DijkstraBbodyPredFailure era)

deriving anyclass instance
  (Era era, NoThunks (PredicateFailure (EraRule "LEDGERS" era))) =>
  NoThunks (DijkstraBbodyPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  EncCBOR (DijkstraBbodyPredFailure era)
  where
  encCBOR = \case
    DijkstraBbodyPredFailure cbpf -> encCBOR cbpf

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (DijkstraBbodyPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraBbodyPred" $ \case
    1 -> SumD DijkstraBbodyPredFailure <! From
    n -> Invalid n

type instance EraRuleFailure "BBODY" DijkstraEra = DijkstraBbodyPredFailure DijkstraEra

type instance EraRuleEvent "BBODY" DijkstraEra = AlonzoBbodyEvent DijkstraEra

instance InjectRuleFailure "BBODY" DijkstraBbodyPredFailure DijkstraEra

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure DijkstraEra where
  injectFailure = alonzoToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertsPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

alonzoToDijkstraBbodyPredFailure ::
  AlonzoBbodyPredFailure era ->
  DijkstraBbodyPredFailure era
alonzoToDijkstraBbodyPredFailure =
  conwayToDijkstraBbodyPredFailure . alonzoToConwayBbodyPredFailure

conwayToDijkstraBbodyPredFailure ::
  ConwayBbodyPredFailure era ->
  DijkstraBbodyPredFailure era
conwayToDijkstraBbodyPredFailure = DijkstraBbodyPredFailure

shelleyToDijkstraBbodyPredFailure ::
  ShelleyBbodyPredFailure era ->
  DijkstraBbodyPredFailure era
shelleyToDijkstraBbodyPredFailure =
  conwayToDijkstraBbodyPredFailure . shelleyToConwayBbodyPredFailure

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , AlonzoEraTxWits era
  , BlockBody era ~ DijkstraBlockBody era
  , EraBlockBody era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , EraRule "BBODY" era ~ DijkstraBBODY era
  , AlonzoEraTx era
  , BabbageEraTxBody era
  , ConwayEraPParams era
  ) =>
  STS (DijkstraBBODY era)
  where
  type State (DijkstraBBODY era) = ShelleyBbodyState era

  type Signal (DijkstraBBODY era) = Block BHeaderView era

  type Environment (DijkstraBBODY era) = BbodyEnv era

  type BaseM (DijkstraBBODY era) = ShelleyBase

  type PredicateFailure (DijkstraBBODY era) = DijkstraBbodyPredFailure era

  type Event (DijkstraBBODY era) = AlonzoBbodyEvent era

  initialRules = []
  transitionRules =
    [ dijkstraBbodyTransition @era
        -- TODO: these needs to be reintroduced
        -- >> conwayBbodyTransition @era
        -- >> alonzoBbodyTransition @era
    ]

dijkstraBbodyTransition ::
  forall era.
  TransitionRule (EraRule "BBODY" era)
dijkstraBbodyTransition = do
  judgmentContext
    >>= \( TRC
             ( _
               , state
               , _
               )
           ) -> do
        pure state

instance
  forall era ledgers.
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  ) =>
  Embed ledgers (DijkstraBBODY era)
  where
  wrapFailed = shelleyToDijkstraBbodyPredFailure . LedgersFailure @era
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent


