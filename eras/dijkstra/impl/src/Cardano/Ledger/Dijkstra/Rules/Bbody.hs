{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxBlockExUnitsL)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Nonce,
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (Block (..), EraBlockHeader (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.BlockBody (
  DijkstraEraBlockBody (..),
  PerasCert,
  PerasKey (..),
  validatePerasCert,
 )
import Cardano.Ledger.Dijkstra.Era (
  DijkstraBBODY,
  DijkstraBbodySignal (..),
  DijkstraEra,
  DijkstraEraBlockHeader (..),
 )
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Shelley.API (incrBlocks)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), utxoL)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Slot (slotToEpochBoundary)
import Control.DeepSeq (NFData)
import Control.State.Transition
import Data.Sequence (Seq)
import Data.Sequence.Strict (fromStrict)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data DijkstraBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch RelEQ (Hash HASH EraIndependentBlockBody))
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure (PredicateFailure (EraRule "LEDGERS" era))
  | TooManyExUnits (Mismatch RelLTEQ ExUnits)
  | BodyRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  | PerasCertValidationFailed PerasCert Nonce
  deriving (Generic)

instance NFData (PredicateFailure (EraRule "LEDGERS" era)) => NFData (DijkstraBbodyPredFailure era)

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGERS" era))) =>
  Show (DijkstraBbodyPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGERS" era))) =>
  Eq (DijkstraBbodyPredFailure era)

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
      PerasCertValidationFailed cert nonce ->
        Sum PerasCertValidationFailed 5 !> To cert !> To nonce

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
    5 -> SumD PerasCertValidationFailed <! From <! From
    n -> Invalid n

type instance EraRuleFailure "BBODY" DijkstraEra = DijkstraBbodyPredFailure DijkstraEra

type instance EraRuleEvent "BBODY" DijkstraEra = Alonzo.AlonzoBbodyEvent DijkstraEra

instance InjectRuleFailure "BBODY" DijkstraBbodyPredFailure DijkstraEra

instance InjectRuleFailure "BBODY" Conway.ConwayBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . Conway.alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . Conway.shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgersPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure . Conway.shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , AlonzoEraTxWits era
  , EraBlockBody era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" Conway.ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , EraRule "BBODY" era ~ DijkstraBBODY era
  , AlonzoEraTx era
  , BabbageEraTxBody era
  , ConwayEraPParams era
  , DijkstraEraBlockBody era
  ) =>
  STS (DijkstraBBODY era)
  where
  type State (DijkstraBBODY era) = Shelley.ShelleyBbodyState era

  type Signal (DijkstraBBODY era) = DijkstraBbodySignal era

  type Environment (DijkstraBBODY era) = Shelley.BbodyEnv era

  type BaseM (DijkstraBBODY era) = ShelleyBase

  type PredicateFailure (DijkstraBBODY era) = DijkstraBbodyPredFailure era

  type Event (DijkstraBBODY era) = Alonzo.AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [dijkstraBbodyTransition @era]

dijkstraBbodyTransition ::
  forall era.
  ( Signal (EraRule "BBODY" era) ~ DijkstraBbodySignal era
  , State (EraRule "BBODY" era) ~ Shelley.ShelleyBbodyState era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , Environment (EraRule "BBODY" era) ~ Shelley.BbodyEnv era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , DijkstraEraBlockBody era
  , BabbageEraTxBody era
  , InjectRuleFailure "BBODY" Conway.ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , STS (EraRule "BBODY" era)
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , AlonzoEraTx era
  , InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , ConwayEraPParams era
  ) =>
  TransitionRule (EraRule "BBODY" era)
dijkstraBbodyTransition = do
  TRC
    ( Shelley.BbodyEnv pp account
      , Shelley.BbodyState ls blocksMade
      , DijkstraBbodySignal block@Block {blockBody}
      ) <-
    judgmentContext

  Shelley.validateBlockBodySize block (pp ^. ppProtocolVersionL)

  Shelley.validateBlockBodyHash block

  let bhSlot = block ^. slotNoBlockHeaderL

  (firstSlot, curEpoch) <- liftSTS $ slotToEpochBoundary bhSlot

  let txs = blockBody ^. txSeqBlockBodyL

  ls' <-
    trans @(EraRule "LEDGERS" era) $
      TRC
        ( Shelley.LedgersEnv bhSlot curEpoch pp account
        , ls
        , fromStrict txs
        )

  Alonzo.validateExUnits @era txs $ pp ^. ppMaxBlockExUnitsL

  Conway.validateBodyRefScriptsSizeTooBig @era pp blockBody (ls ^. utxoL)

  case blockBody ^. perasCertBlockBodyL of
    SNothing -> pure ()
    SJust cert ->
      let nonce = block ^. prevNonceBlockHeaderL
       in validatePerasCert nonce PerasKey cert ?! injectFailure (PerasCertValidationFailed cert nonce)

  pure $ Shelley.BbodyState ls' $ incrBlocks block firstSlot (pp ^. ppDG) blocksMade

-- | Validate that Peras certificate is in the block body.
conwayToDijkstraBbodyPredFailure ::
  forall era. Conway.ConwayBbodyPredFailure era -> DijkstraBbodyPredFailure era
conwayToDijkstraBbodyPredFailure = \case
  Conway.WrongBlockBodySizeBBODY mm -> WrongBlockBodySizeBBODY mm
  Conway.InvalidBodyHashBBODY mm -> InvalidBodyHashBBODY mm
  Conway.LedgersFailure f -> LedgersFailure f
  Conway.TooManyExUnits mm -> TooManyExUnits mm
  Conway.BodyRefScriptsSizeTooBig mm -> BodyRefScriptsSizeTooBig mm
  Conway.HeaderProtVerTooHigh {} -> error "Impossible: HeaderProtVerTooHigh cannot be triggered in Dijkstra era"

instance
  ( BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  ) =>
  Embed ledgers (DijkstraBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = Alonzo.ShelleyInAlonzoEvent . Shelley.LedgersEvent
