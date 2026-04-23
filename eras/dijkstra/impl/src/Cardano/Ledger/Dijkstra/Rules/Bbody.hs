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
{-# LANGUAGE TupleSections #-}
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
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxBlockExUnitsL)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyEvent (ShelleyInAlonzoEvent),
  AlonzoBbodyPredFailure,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
  validateExUnits,
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Nonce,
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord,
  invalidKey,
 )
import Cardano.Ledger.Block (Block (..), EraBlockHeader (..))
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
    \case
      WrongBlockBodySizeBBODY mm -> encodeListLen 2 <> encodeWord 0 <> encCBOR mm
      InvalidBodyHashBBODY mm -> encodeListLen 2 <> encodeWord 1 <> encCBOR mm
      LedgersFailure x -> encodeListLen 2 <> encodeWord 2 <> encCBOR x
      TooManyExUnits mm -> encodeListLen 2 <> encodeWord 3 <> encCBOR mm
      BodyRefScriptsSizeTooBig mm -> encodeListLen 2 <> encodeWord 4 <> encCBOR mm
      PerasCertValidationFailed cert nonce ->
        encodeListLen 3 <> encodeWord 5 <> encCBOR cert <> encCBOR nonce

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (DijkstraBbodyPredFailure era)
  where
  decCBOR = decodeRecordSum "ConwayBbodyPred" $ \case
    0 -> fmap (2,) $ WrongBlockBodySizeBBODY <$> decCBOR
    1 -> fmap (2,) $ InvalidBodyHashBBODY <$> decCBOR
    2 -> fmap (2,) $ LedgersFailure <$> decCBOR
    3 -> fmap (2,) $ TooManyExUnits <$> decCBOR
    4 -> fmap (2,) $ BodyRefScriptsSizeTooBig <$> decCBOR
    5 -> fmap (3,) $ PerasCertValidationFailed <$> decCBOR <*> decCBOR
    n -> invalidKey n

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
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  , EraRule "BBODY" era ~ DijkstraBBODY era
  , AlonzoEraTx era
  , BabbageEraTxBody era
  , ConwayEraPParams era
  , DijkstraEraBlockBody era
  ) =>
  STS (DijkstraBBODY era)
  where
  type State (DijkstraBBODY era) = ShelleyBbodyState era

  type Signal (DijkstraBBODY era) = DijkstraBbodySignal era

  type Environment (DijkstraBBODY era) = BbodyEnv era

  type BaseM (DijkstraBBODY era) = ShelleyBase

  type PredicateFailure (DijkstraBBODY era) = DijkstraBbodyPredFailure era

  type Event (DijkstraBBODY era) = AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [dijkstraBbodyTransition @era]

dijkstraBbodyTransition ::
  forall era.
  ( Signal (EraRule "BBODY" era) ~ DijkstraBbodySignal era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , DijkstraEraBlockBody era
  , BabbageEraTxBody era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  , STS (EraRule "BBODY" era)
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , AlonzoEraTx era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , ConwayEraPParams era
  ) =>
  TransitionRule (EraRule "BBODY" era)
dijkstraBbodyTransition = do
  TRC (BbodyEnv pp account, BbodyState ls blocksMade, DijkstraBbodySignal block@Block {blockBody}) <-
    judgmentContext

  Shelley.validateBlockBodySize block (pp ^. ppProtocolVersionL)

  Shelley.validateBlockBodyHash block

  let bhSlot = block ^. slotNoBlockHeaderL

  (firstSlot, curEpoch) <- liftSTS $ slotToEpochBoundary bhSlot

  let txs = blockBody ^. txSeqBlockBodyL

  ls' <-
    trans @(EraRule "LEDGERS" era) $
      TRC
        ( LedgersEnv bhSlot curEpoch pp account
        , ls
        , fromStrict txs
        )

  validateExUnits @era txs $ pp ^. ppMaxBlockExUnitsL

  Conway.validateBodyRefScriptsSizeTooBig @era pp blockBody (ls ^. utxoL)

  case blockBody ^. perasCertBlockBodyL of
    SNothing -> pure ()
    SJust cert ->
      let nonce = block ^. prevNonceBlockHeaderL
       in validatePerasCert nonce PerasKey cert ?! injectFailure (PerasCertValidationFailed cert nonce)

  pure $ BbodyState ls' $ incrBlocks block firstSlot (pp ^. ppDG) blocksMade

-- | Validate that Peras certificate is in the block body.
conwayToDijkstraBbodyPredFailure ::
  forall era. ConwayBbodyPredFailure era -> DijkstraBbodyPredFailure era
conwayToDijkstraBbodyPredFailure = \case
  Conway.WrongBlockBodySizeBBODY mm -> WrongBlockBodySizeBBODY mm
  Conway.InvalidBodyHashBBODY mm -> InvalidBodyHashBBODY mm
  Conway.LedgersFailure f -> LedgersFailure f
  Conway.TooManyExUnits mm -> TooManyExUnits mm
  Conway.BodyRefScriptsSizeTooBig mm -> BodyRefScriptsSizeTooBig mm
  Conway.HeaderProtVerTooHigh {} -> error "Impossible: HeaderProtVerTooHigh cannot be triggered in Dijkstra era"

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
