{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 910
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/27342
{-# OPTIONS_GHC -fno-spec-eval #-}
#endif

module Cardano.Ledger.Conway.Rules.Bbody (
  BBODY,
  ConwayBbodyPredFailure (..),
  alonzoToConwayBbodyPredFailure,
  shelleyToConwayBbodyPredFailure,
  bbodyTransition,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Network (..),
  Relation (..),
  ShelleyBase,
  Version,
  networkId,
  pvMajor,
  succVersion,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (
  Block (..),
  EraBlockHeader (..),
  PraosBbodySignal (..),
  PraosEraBlockHeader (..),
 )
import Cardano.Ledger.Conway.Era (BBODY, ConwayEra)
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Ledgers (ConwayLedgersPredFailure (TooManyExUnits))
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.BlockBody (incrBlocks)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Slot (slotToEpochBoundary)
import Control.DeepSeq (NFData)
import Control.Monad (guard, when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failOnJust,
  judgmentContext,
  liftSTS,
  trans,
 )
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Lens.Micro

data ConwayBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch RelEQ (Hash HASH EraIndependentBlockBody))
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure (PredicateFailure (EraRule "LEDGERS" era))
  | HeaderProtVerTooHigh (Mismatch RelLTEQ Version)
  deriving (Generic)

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGERS" era))) =>
  Show (ConwayBbodyPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGERS" era))) =>
  Eq (ConwayBbodyPredFailure era)

deriving instance
  (Era era, Ord (PredicateFailure (EraRule "LEDGERS" era))) =>
  Ord (ConwayBbodyPredFailure era)

deriving anyclass instance
  (Era era, NFData (PredicateFailure (EraRule "LEDGERS" era))) =>
  NFData (ConwayBbodyPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  EncCBOR (ConwayBbodyPredFailure era)
  where
  encCBOR =
    encode . \case
      WrongBlockBodySizeBBODY mm -> Sum WrongBlockBodySizeBBODY 0 !> ToGroup mm
      InvalidBodyHashBBODY mm -> Sum (InvalidBodyHashBBODY @era) 1 !> ToGroup mm
      LedgersFailure x -> Sum (LedgersFailure @era) 2 !> To x
      HeaderProtVerTooHigh mm -> Sum HeaderProtVerTooHigh 5 !> To mm

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (ConwayBbodyPredFailure era)
  where
  decCBOR = decode . Summands "ConwayBbodyPred" $ \case
    0 -> SumD WrongBlockBodySizeBBODY <! FromGroup
    1 -> SumD InvalidBodyHashBBODY <! FromGroup
    2 -> SumD LedgersFailure <! From
    5 -> SumD HeaderProtVerTooHigh <! From
    n -> Invalid n

type instance EraRuleFailure "BBODY" ConwayEra = ConwayBbodyPredFailure ConwayEra

type instance EraRuleEvent "BBODY" ConwayEra = Alonzo.AlonzoBbodyEvent ConwayEra

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure ConwayEra

instance InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure ConwayEra where
  injectFailure = alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ConwayLedgersPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Babbage.BabbageUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Babbage.BabbageUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxosPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Allegra.AllegraUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertsPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPoolPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovCertPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

shelleyToConwayBbodyPredFailure ::
  forall era.
  Shelley.ShelleyBbodyPredFailure era ->
  ConwayBbodyPredFailure era
shelleyToConwayBbodyPredFailure
  (Shelley.WrongBlockBodySizeBBODY m) =
    WrongBlockBodySizeBBODY m
shelleyToConwayBbodyPredFailure
  (Shelley.InvalidBodyHashBBODY m) =
    InvalidBodyHashBBODY m
shelleyToConwayBbodyPredFailure (Shelley.LedgersFailure x) = LedgersFailure x

alonzoToConwayBbodyPredFailure ::
  forall era.
  InjectRuleFailure "LEDGERS" ConwayLedgersPredFailure era =>
  Alonzo.AlonzoBbodyPredFailure era ->
  ConwayBbodyPredFailure era
alonzoToConwayBbodyPredFailure (Alonzo.ShelleyInAlonzoBbodyPredFailure x) = shelleyToConwayBbodyPredFailure x
alonzoToConwayBbodyPredFailure (Alonzo.TooManyExUnits m) = LedgersFailure $ injectFailure (TooManyExUnits m)

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , EraBlockBody era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , EraRule "BBODY" era ~ BBODY era
  ) =>
  STS (BBODY era)
  where
  type State (BBODY era) = Shelley.ShelleyBbodyState era

  type Signal (BBODY era) = PraosBbodySignal era

  type Environment (BBODY era) = Shelley.BbodyEnv era

  type BaseM (BBODY era) = ShelleyBase

  type PredicateFailure (BBODY era) = ConwayBbodyPredFailure era

  type Event (BBODY era) = Alonzo.AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( Signal (EraRule "BBODY" era) ~ PraosBbodySignal era
  , State (EraRule "BBODY" era) ~ Shelley.ShelleyBbodyState era
  , Environment (EraRule "BBODY" era) ~ Shelley.BbodyEnv era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , STS (EraRule "BBODY" era)
  , EraBlockBody era
  ) =>
  TransitionRule (EraRule "BBODY" era)
bbodyTransition = do
  TRC
    ( Shelley.BbodyEnv pp account
      , Shelley.BbodyState ls blocksMade
      , PraosBbodySignal block@Block {blockBody}
      ) <-
    judgmentContext

  let curProtVerMajor = pvMajor $ pp ^. ppProtocolVersionL
      checkHeaderProtVerTooHigh = do
        let bhProtVerMajor = pvMajor $ block ^. protVerBlockHeaderL

        -- There is always next version higher than the current one used
        nextProtVerMajor <- succVersion curProtVerMajor
        -- If header version is less than or equal to the next version, then we are OK.
        guard (bhProtVerMajor > nextProtVerMajor)
        Just $
          Mismatch
            { mismatchSupplied = bhProtVerMajor
            , mismatchExpected = nextProtVerMajor
            }
  netId <- liftSTS $ asks networkId

  when (netId == Mainnet) $
    failOnJust checkHeaderProtVerTooHigh $
      injectFailure . HeaderProtVerTooHigh @era

  Shelley.validateBlockBodySize block (pp ^. ppProtocolVersionL)

  Shelley.validateBlockBodyHash block

  let bhSlot = block ^. slotNoBlockHeaderL

  (firstSlot, curEpoch) <- liftSTS $ slotToEpochBoundary bhSlot

  ls' <-
    trans @(EraRule "LEDGERS" era) $
      TRC
        ( Shelley.LedgersEnv bhSlot curEpoch pp account
        , ls
        , StrictSeq.fromStrict $ blockBody ^. txSeqBlockBodyL
        )

  pure $ Shelley.BbodyState ls' $ incrBlocks block firstSlot (pp ^. ppDG) blocksMade

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  ) =>
  Embed ledgers (BBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = Alonzo.ShelleyInAlonzoEvent . Shelley.LedgersEvent
