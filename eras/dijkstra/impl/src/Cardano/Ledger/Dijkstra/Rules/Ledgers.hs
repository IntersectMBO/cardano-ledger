{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Cardano.Ledger.Dijkstra.Rules.Ledgers (
  LEDGERS,
  DijkstraLedgersPredFailure (..),
  validateBlockExUnits,
  validateBlockRefScriptsSize,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxBlockExUnitsL)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.Scripts (ExUnits, OrdExUnits (..), pointWiseExUnits)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.BlockBody (ConwayBlockBodyLimits (..))
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppMaxRefScriptSizePerBlockG)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, LEDGER, LEDGERS)
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent, DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..))
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  LedgerStateWithLimits (..),
  lswlBlockBodyLimitsL,
  lswlLedgerStateL,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import Data.Default (Default)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Validation (failureUnless)

data DijkstraLedgersPredFailure era
  = -- | LEDGER rule subtransition failures
    LedgerFailureInLedgers (PredicateFailure (EraRule "LEDGER" era))
  | -- | Total execution units of all transactions exceed the block limit
    TooManyExUnits (Mismatch RelLTEQ OrdExUnits)
  | -- | Total reference script size of all transactions exceeds the block limit
    BodyRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  deriving (Generic)

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGER" era))) =>
  Show (DijkstraLedgersPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGER" era))) =>
  Eq (DijkstraLedgersPredFailure era)

deriving instance
  (Era era, Ord (PredicateFailure (EraRule "LEDGER" era))) =>
  Ord (DijkstraLedgersPredFailure era)

instance
  NFData (PredicateFailure (EraRule "LEDGER" era)) =>
  NFData (DijkstraLedgersPredFailure era)

instance
  (Era era, EncCBOR (PredicateFailure (EraRule "LEDGER" era))) =>
  EncCBOR (DijkstraLedgersPredFailure era)
  where
  encCBOR =
    encode . \case
      LedgerFailureInLedgers x -> Sum (LedgerFailureInLedgers @era) 0 !> To x
      TooManyExUnits mm -> Sum TooManyExUnits 1 !> To mm
      BodyRefScriptsSizeTooBig mm -> Sum BodyRefScriptsSizeTooBig 2 !> To mm

instance
  (Era era, DecCBOR (PredicateFailure (EraRule "LEDGER" era))) =>
  DecCBOR (DijkstraLedgersPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraLedgersPredFailure" $ \case
    0 -> SumD LedgerFailureInLedgers <! From
    1 -> SumD TooManyExUnits <! From
    2 -> SumD BodyRefScriptsSizeTooBig <! From
    n -> Invalid n

type instance EraRuleFailure "LEDGERS" DijkstraEra = DijkstraLedgersPredFailure DijkstraEra

type instance EraRuleEvent "LEDGERS" DijkstraEra = Shelley.ShelleyLedgersEvent DijkstraEra

instance InjectRuleFailure "LEDGERS" DijkstraLedgersPredFailure DijkstraEra

instance InjectRuleFailure "LEDGERS" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers

instance InjectRuleFailure "LEDGERS" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" DijkstraGovPredFailure DijkstraEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance
  ( ApplyTx era
  , EraBlockBody era
  , EraGov era
  , EraStake era
  , Default (CertState era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , BlockBodyLimits era ~ ConwayBlockBodyLimits era
  , ConwayEraPParams era
  , EraRule "LEDGERS" era ~ LEDGERS era
  , InjectRuleFailure "LEDGERS" DijkstraLedgersPredFailure era
  , Default (LedgerState era)
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerStateWithLimits era
  type Signal (LEDGERS era) = Seq (Tx TopTx era)
  type Environment (LEDGERS era) = Shelley.ShelleyLedgersEnv era
  type BaseM (LEDGERS era) = ShelleyBase
  type PredicateFailure (LEDGERS era) = DijkstraLedgersPredFailure era
  type Event (LEDGERS era) = Shelley.ShelleyLedgersEvent era

  transitionRules = [ledgersTransition]

ledgersTransition ::
  forall era.
  ( ApplyTx era
  , EraBlockBody era
  , EraGov era
  , EraStake era
  , Default (CertState era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ Shelley.LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , BlockBodyLimits era ~ ConwayBlockBodyLimits era
  , ConwayEraPParams era
  , EraRule "LEDGERS" era ~ LEDGERS era
  , InjectRuleFailure "LEDGERS" DijkstraLedgersPredFailure era
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (Shelley.LedgersEnv slot epochNo pp account, initLedgerState, txs) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  (finalLedgerState, _) <-
    foldM -- cache is lazy on purpose, since it is unused when validation is off
      ( \acc@(!curLedgerState, stAnnTxCache) (ix, tx) ->
          -- Skip the remaining transactions once one has failed, so that a block is
          -- rejected on its first failure rather than on all of them.
          whenFailureFreeDefault acc $ do
            let
              utxo = curLedgerState ^. lswlLedgerStateL . utxoL
              stAnnTx = mkStAnnTx ei sysStart pp utxo stAnnTxCache tx
            newLedgerState <-
              trans @(EraRule "LEDGER" era) $
                TRC (Shelley.LedgerEnv slot (Just epochNo) ix pp account, curLedgerState, stAnnTx)
            let ConwayBlockBodyLimits {cbblExUnits, cbblRefScriptsSize} =
                  newLedgerState ^. lswlBlockBodyLimitsL
            runTest $ validateBlockExUnits pp cbblExUnits
            runTest $ validateBlockRefScriptsSize pp cbblRefScriptsSize
            pure (newLedgerState, stAnnTx ^. cacheStAnnTxG)
      )
      (initLedgerState, mempty)
      $ zip [minBound ..]
      $ toList txs
  pure finalLedgerState

validateBlockExUnits ::
  AlonzoEraPParams era => PParams era -> ExUnits -> Test (DijkstraLedgersPredFailure era)
validateBlockExUnits pp exUnits =
  let maxExUnits = pp ^. ppMaxBlockExUnitsL
   in failureUnless (pointWiseExUnits (<=) exUnits maxExUnits) $
        TooManyExUnits
          Mismatch
            { mismatchSupplied = OrdExUnits exUnits
            , mismatchExpected = OrdExUnits maxExUnits
            }

validateBlockRefScriptsSize ::
  ConwayEraPParams era => PParams era -> Int -> Test (DijkstraLedgersPredFailure era)
validateBlockRefScriptsSize pp refScriptsSize =
  let maxRefScriptsSize = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG
   in failureUnless (refScriptsSize <= maxRefScriptsSize) $
        BodyRefScriptsSizeTooBig
          Mismatch
            { mismatchSupplied = refScriptsSize
            , mismatchExpected = maxRefScriptsSize
            }

instance
  ( STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ DijkstraLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ DijkstraLedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailureInLedgers
  wrapEvent = Shelley.LedgerEvent
