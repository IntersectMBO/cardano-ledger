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

module Cardano.Ledger.Conway.Rules.Ledgers (
  LEDGERS,
  ConwayLedgersPredFailure (..),
  validateBodyRefScriptsSizeTooBig,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.Scripts (OrdExUnits (..))
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.BlockBody (ConwayBlockBodyLimits (..))
import Cardano.Ledger.Conway.Era (ConwayEra, LEDGER, LEDGERS)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerEvent, ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Core
import Cardano.Ledger.Rules.ValidationMode
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

data ConwayLedgersPredFailure era
  = LedgerFailureInLedgers (PredicateFailure (EraRule "LEDGER" era))
  | -- | Total execution units of all transactions exceed the block limit
    TooManyExUnits (Mismatch RelLTEQ OrdExUnits)
  | -- | Total reference script size of all transactions exceeds the block limit
    BodyRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  deriving (Generic)

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGER" era))) =>
  Show (ConwayLedgersPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGER" era))) =>
  Eq (ConwayLedgersPredFailure era)

deriving instance
  (Era era, Ord (PredicateFailure (EraRule "LEDGER" era))) =>
  Ord (ConwayLedgersPredFailure era)

instance
  NFData (PredicateFailure (EraRule "LEDGER" era)) =>
  NFData (ConwayLedgersPredFailure era)

instance
  (Era era, EncCBOR (PredicateFailure (EraRule "LEDGER" era))) =>
  EncCBOR (ConwayLedgersPredFailure era)
  where
  encCBOR =
    encode . \case
      LedgerFailureInLedgers x -> Sum (LedgerFailureInLedgers @era) 0 !> To x
      TooManyExUnits mm -> Sum TooManyExUnits 1 !> To mm
      BodyRefScriptsSizeTooBig mm -> Sum BodyRefScriptsSizeTooBig 2 !> To mm

instance
  (Era era, DecCBOR (PredicateFailure (EraRule "LEDGER" era))) =>
  DecCBOR (ConwayLedgersPredFailure era)
  where
  decCBOR = decode . Summands "ConwayLedgersPredFailure" $ \case
    0 -> SumD LedgerFailureInLedgers <! From
    1 -> SumD TooManyExUnits <! From
    2 -> SumD BodyRefScriptsSizeTooBig <! From
    n -> Invalid n

type instance EraRuleFailure "LEDGERS" ConwayEra = ConwayLedgersPredFailure ConwayEra

type instance EraRuleEvent "LEDGERS" ConwayEra = Shelley.ShelleyLedgersEvent ConwayEra

instance InjectRuleFailure "LEDGERS" ConwayLedgersPredFailure ConwayEra

instance InjectRuleFailure "LEDGERS" ConwayLedgerPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers

instance InjectRuleFailure "LEDGERS" ConwayUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxowPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Babbage.BabbageUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Alonzo.AlonzoUtxosPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayUtxosPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Allegra.AllegraUtxoPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertsPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayCertPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayDelegPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" Shelley.ShelleyPoolPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovCertPredFailure ConwayEra where
  injectFailure = LedgerFailureInLedgers . injectFailure

instance InjectRuleFailure "LEDGERS" ConwayGovPredFailure ConwayEra where
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
  ) =>
  STS (LEDGERS era)
  where
  type State (LEDGERS era) = LedgerState era
  type Signal (LEDGERS era) = Seq (Tx TopTx era)
  type Environment (LEDGERS era) = Shelley.ShelleyLedgersEnv era
  type BaseM (LEDGERS era) = ShelleyBase
  type PredicateFailure (LEDGERS era) = ConwayLedgersPredFailure era
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
  ) =>
  TransitionRule (LEDGERS era)
ledgersTransition = do
  TRC (Shelley.LedgersEnv slot epochNo pp account, initLedgerState, txs) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  (LedgerStateWithLimits finalLedgerState _, _) <-
    foldM -- cache is lazy on purpose, since it is unused when validation is off
      ( \acc@(!curLedgerState, stAnnTxCache) (ix, tx) ->
          -- Skip the remaining transactions once one has failed
          whenFailureFreeDefault acc $ do
            let
              utxo = curLedgerState ^. lswlLedgerStateL . utxoL
              stAnnTx = mkStAnnTx ei sysStart pp utxo stAnnTxCache tx
            newLedgerState <-
              trans @(EraRule "LEDGER" era) $
                TRC (Shelley.LedgerEnv slot (Just epochNo) ix pp account, curLedgerState, stAnnTx)
            let ConwayBlockBodyLimits {cbblExUnits, cbblRefScriptsSize} =
                  newLedgerState ^. lswlBlockBodyLimitsL
            validateTrans TooManyExUnits $
              Alonzo.validateExUnits pp cbblExUnits
            validateTrans BodyRefScriptsSizeTooBig $
              validateBodyRefScriptsSizeTooBig pp cbblRefScriptsSize
            pure (newLedgerState, stAnnTx ^. cacheStAnnTxG)
      )
      (LedgerStateWithLimits initLedgerState mempty, mempty)
      $ zip [minBound ..]
      $ toList txs
  pure finalLedgerState

instance
  ( STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  ) =>
  Embed (LEDGER era) (LEDGERS era)
  where
  wrapFailed = LedgerFailureInLedgers
  wrapEvent = Shelley.LedgerEvent

-- | Validate that total reference script size does not exceed block limit.
validateBodyRefScriptsSizeTooBig ::
  ConwayEraPParams era =>
  PParams era ->
  -- | Total reference script size of all transactions in the block
  Int ->
  Test (Mismatch RelLTEQ Int)
validateBodyRefScriptsSizeTooBig pp totalSize =
  let maxSize = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG
   in failureUnless (totalSize <= maxSize) $
        Mismatch
          { mismatchSupplied = totalSize
          , mismatchExpected = maxSize
          }
