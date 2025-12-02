{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Cardano.Ledger.Dijkstra.Rules.Bbody (
  DijkstraBBODY,
  DijkstraBbodyPredFailure (..),
  conwayToDijkstraBbodyPredFailure,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyEvent (ShelleyInAlonzoEvent),
  AlonzoBbodyPredFailure,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
  alonzoBbodyTransition,
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Relation (..),
  ShelleyBase,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure,
  ConwayCertPredFailure,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayGovPredFailure,
  ConwayUtxosPredFailure,
  alonzoToConwayBbodyPredFailure,
  shelleyToConwayBbodyPredFailure,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraBBODY, DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyPredFailure,
  ShelleyBbodyState (..),
  ShelleyLedgersEnv (..),
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (
  Embed (..),
  STS (..),
 )
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data DijkstraBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch RelEQ (Hash HASH EraIndependentBlockBody))
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure (PredicateFailure (EraRule "LEDGERS" era))
  | TooManyExUnits (Mismatch RelLTEQ ExUnits)
  | BodyRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
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
  encCBOR =
    encode . \case
      WrongBlockBodySizeBBODY mm -> Sum WrongBlockBodySizeBBODY 0 !> To mm
      InvalidBodyHashBBODY mm -> Sum (InvalidBodyHashBBODY @era) 1 !> To mm
      LedgersFailure x -> Sum (LedgersFailure @era) 2 !> To x
      TooManyExUnits mm -> Sum TooManyExUnits 3 !> To mm
      BodyRefScriptsSizeTooBig mm -> Sum BodyRefScriptsSizeTooBig 4 !> To mm

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (DijkstraBbodyPredFailure era)
  where
  decCBOR = decode . Summands "ConwayBbodyPred" $ \case
    0 -> SumD WrongBlockBodySizeBBODY <! From
    1 -> SumD InvalidBodyHashBBODY <! From
    2 -> SumD LedgersFailure <! From
    3 -> SumD TooManyExUnits <! From
    4 -> SumD BodyRefScriptsSizeTooBig <! From
    n -> Invalid n

type instance EraRuleFailure "BBODY" DijkstraEra = DijkstraBbodyPredFailure DijkstraEra

type instance EraRuleEvent "BBODY" DijkstraEra = AlonzoBbodyEvent DijkstraEra

instance InjectRuleFailure "BBODY" DijkstraBbodyPredFailure DijkstraEra

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertsPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , AlonzoEraTxWits era
  , EraBlockBody era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
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
  transitionRules = [Conway.conwayBbodyTransition @era >> alonzoBbodyTransition @era]

conwayToDijkstraBbodyPredFailure ::
  forall era. ConwayBbodyPredFailure era -> DijkstraBbodyPredFailure era
conwayToDijkstraBbodyPredFailure = \case
  Conway.WrongBlockBodySizeBBODY mm -> WrongBlockBodySizeBBODY mm
  Conway.InvalidBodyHashBBODY mm -> InvalidBodyHashBBODY mm
  Conway.LedgersFailure f -> LedgersFailure f
  Conway.TooManyExUnits mm -> TooManyExUnits mm
  Conway.BodyRefScriptsSizeTooBig mm -> BodyRefScriptsSizeTooBig mm

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  ) =>
  Embed ledgers (DijkstraBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInAlonzoEvent . Shelley.LedgersEvent
