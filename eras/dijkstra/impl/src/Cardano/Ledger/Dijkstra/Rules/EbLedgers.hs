{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Cardano.Ledger.Dijkstra.Rules.EbLedgers (
  EBLEDGERS,
  EbLedgersPredFailure (..),
  EbTxsSizeTooBig (..),
  EbExUnitsTooBig (..),
  EbRefScriptsSizeTooBig (..),
  validateEbTxsSize,
  validateEbExUnits,
  validateEbRefScriptsSize,
) where

import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Conway.BlockBody (ConwayBlockBodyLimits (..))
import Cardano.Ledger.Dijkstra.Block.Endorser (
  EbTransactions (..),
  EraEndorserBlock (..),
 )
import Cardano.Ledger.Dijkstra.Core (
  Era,
  EraBlockBody (..),
  EraGov,
  EraRule,
  EraRuleFailure,
  EraTx (..),
  EraTxOut,
  InjectRuleFailure (..),
  PParams,
  TxLevel (..),
 )
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, EBLEDGERS, LEDGER)
import Cardano.Ledger.Dijkstra.PParams (
  DijkstraEraPParams,
  ppMaxEndorserBlockExUnitsL,
  ppMaxEndorserBlockTxsSizeL,
  ppMaxRefScriptSizePerEndorserBlockL,
 )
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerEvent, DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.State (CanSetUTxO (..), EraStake)
import Cardano.Ledger.Plutus.ExUnits (ExUnits, OrdExUnits (..), pointWiseExUnits)
import Cardano.Ledger.Rules.ValidationMode (Test)
import Cardano.Ledger.Shelley.API (
  ApplyTx (..),
  Globals (..),
  LedgerEnv (..),
  ShelleyLedgersEnv (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState,
  LedgerStateWithLimits (..),
  lswlBlockBodyLimitsL,
  lswlLedgerStateL,
 )
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  validateTrans,
  whenFailureFreeDefault,
 )
import Data.Foldable (Foldable (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import Validation (failureUnless)

newtype EbTxsSizeTooBig era = EbTxsSizeTooBig (Mismatch RelLTEQ Word32)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbExUnitsTooBig era = EbExUnitsTooBig (Mismatch RelLTEQ OrdExUnits)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

newtype EbRefScriptsSizeTooBig era = EbRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, NFData, EncCBOR, DecCBOR)

data EbLedgersPredFailure era
  = EbLedgersLedgerFailure (PredicateFailure (EraRule "LEDGER" era))
  | EbLedgersEbTxsSizeTooBig (EbTxsSizeTooBig era)
  | EbLedgersEbExUnitsTooBig (EbExUnitsTooBig era)
  | EbLedgersEbRefScriptsSizeTooBig (EbRefScriptsSizeTooBig era)
  deriving (Generic)

deriving instance Eq (PredicateFailure (EraRule "LEDGER" era)) => Eq (EbLedgersPredFailure era)

deriving instance Ord (PredicateFailure (EraRule "LEDGER" era)) => Ord (EbLedgersPredFailure era)

deriving instance Show (PredicateFailure (EraRule "LEDGER" era)) => Show (EbLedgersPredFailure era)

instance NFData (PredicateFailure (EraRule "LEDGER" era)) => NFData (EbLedgersPredFailure era)

instance
  EncCBOR (PredicateFailure (EraRule "LEDGER" era)) =>
  EncCBOR (EbLedgersPredFailure era)
  where
  encCBOR =
    encode . \case
      EbLedgersLedgerFailure f -> Sum (EbLedgersLedgerFailure @era) 0 !> To f
      EbLedgersEbTxsSizeTooBig f -> Sum EbLedgersEbTxsSizeTooBig 1 !> To f
      EbLedgersEbExUnitsTooBig f -> Sum EbLedgersEbExUnitsTooBig 2 !> To f
      EbLedgersEbRefScriptsSizeTooBig f -> Sum EbLedgersEbRefScriptsSizeTooBig 3 !> To f

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  DecCBOR (EbLedgersPredFailure era)
  where
  decCBOR = decode $ Summands "EbLedgersPredFailure" $ \case
    0 -> SumD EbLedgersLedgerFailure <! From
    1 -> SumD EbLedgersEbTxsSizeTooBig <! From
    2 -> SumD EbLedgersEbExUnitsTooBig <! From
    3 -> SumD EbLedgersEbRefScriptsSizeTooBig <! From
    k -> Invalid k

instance
  ( EraTxOut era
  , EraStake era
  , EraGov era
  , EraEndorserBlock era
  , ApplyTx era
  , BaseM (EraRule "LEDGER" era) ~ ShelleyBase
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , Embed (EraRule "LEDGER" era) (EBLEDGERS era)
  , BlockBodyLimits era ~ ConwayBlockBodyLimits era
  , DijkstraEraPParams era
  ) =>
  STS (EBLEDGERS era)
  where
  type Environment (EBLEDGERS era) = ShelleyLedgersEnv era
  type State (EBLEDGERS era) = LedgerState era
  type Signal (EBLEDGERS era) = EndorserBlock era

  type PredicateFailure (EBLEDGERS era) = EbLedgersPredFailure era
  type BaseM (EBLEDGERS era) = ShelleyBase
  type Event (EBLEDGERS era) = DijkstraLedgerEvent era

  initialRules = []
  transitionRules = [ebLedgersTransition]

ebLedgersTransition ::
  forall era.
  ( ApplyTx era
  , EraGov era
  , EraStake era
  , Embed (EraRule "LEDGER" era) (EBLEDGERS era)
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerStateWithLimits era
  , Signal (EraRule "LEDGER" era) ~ StAnnTx TopTx era
  , EraEndorserBlock era
  , BlockBodyLimits era ~ ConwayBlockBodyLimits era
  , DijkstraEraPParams era
  ) =>
  TransitionRule (EBLEDGERS era)
ebLedgersTransition = do
  TRC (LedgersEnv slot epochNo pp account, initLedgerState, endorserBlock) <- judgmentContext
  ei <- liftSTS $ asks epochInfo
  sysStart <- liftSTS $ asks systemStart
  (LedgerStateWithLimits finalLedgerState _, _) <-
    foldM
      ( \acc@(!curLedgerState, stAnnTxCache) (ix, tx) ->
          whenFailureFreeDefault acc $ do
            let utxo = curLedgerState ^. lswlLedgerStateL . utxoL
                stAnnTx = mkStAnnTx ei sysStart pp utxo stAnnTxCache tx
            newLedgerState <-
              trans @(EraRule "LEDGER" era) $
                TRC (LedgerEnv slot (Just epochNo) ix pp account, curLedgerState, stAnnTx)
            let ConwayBlockBodyLimits {cbblTxsSize, cbblExUnits, cbblRefScriptsSize} =
                  newLedgerState ^. lswlBlockBodyLimitsL
            validateTrans EbLedgersEbTxsSizeTooBig $ validateEbTxsSize pp cbblTxsSize
            validateTrans EbLedgersEbExUnitsTooBig $ validateEbExUnits pp cbblExUnits
            validateTrans EbLedgersEbRefScriptsSizeTooBig $
              validateEbRefScriptsSize pp cbblRefScriptsSize
            pure (newLedgerState, stAnnTx ^. cacheStAnnTxG)
      )
      (LedgerStateWithLimits initLedgerState mempty, mempty)
      $ zip [minBound ..] (toList . unEbTransactions $ endorserBlock ^. transactionsEndorserBlockL)
  pure finalLedgerState

validateEbTxsSize ::
  DijkstraEraPParams era => PParams era -> Word32 -> Test (EbTxsSizeTooBig era)
validateEbTxsSize pp txsSize =
  let maxTxsSize = pp ^. ppMaxEndorserBlockTxsSizeL
   in failureUnless (txsSize <= maxTxsSize) $
        EbTxsSizeTooBig
          Mismatch
            { mismatchSupplied = txsSize
            , mismatchExpected = maxTxsSize
            }

validateEbExUnits ::
  DijkstraEraPParams era => PParams era -> ExUnits -> Test (EbExUnitsTooBig era)
validateEbExUnits pp exUnits =
  let maxExUnits = pp ^. ppMaxEndorserBlockExUnitsL
   in failureUnless (pointWiseExUnits (<=) exUnits (unOrdExUnits maxExUnits)) $
        EbExUnitsTooBig
          Mismatch
            { mismatchSupplied = OrdExUnits exUnits
            , mismatchExpected = maxExUnits
            }

validateEbRefScriptsSize ::
  DijkstraEraPParams era => PParams era -> Int -> Test (EbRefScriptsSizeTooBig era)
validateEbRefScriptsSize pp refScriptsSize =
  let maxRefScriptsSize = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerEndorserBlockL
   in failureUnless (refScriptsSize <= maxRefScriptsSize) $
        EbRefScriptsSizeTooBig
          Mismatch
            { mismatchSupplied = refScriptsSize
            , mismatchExpected = maxRefScriptsSize
            }

type instance EraRuleFailure "EBLEDGERS" DijkstraEra = EbLedgersPredFailure DijkstraEra

instance InjectRuleFailure "EBLEDGERS" EbLedgersPredFailure DijkstraEra

instance InjectRuleFailure "EBLEDGERS" EbTxsSizeTooBig DijkstraEra where
  injectFailure = EbLedgersEbTxsSizeTooBig

instance InjectRuleFailure "EBLEDGERS" EbExUnitsTooBig DijkstraEra where
  injectFailure = EbLedgersEbExUnitsTooBig

instance InjectRuleFailure "EBLEDGERS" EbRefScriptsSizeTooBig DijkstraEra where
  injectFailure = EbLedgersEbRefScriptsSizeTooBig

instance
  ( STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ DijkstraLedgerPredFailure era
  ) =>
  Embed (LEDGER era) (EBLEDGERS era)
  where
  wrapFailed = EbLedgersLedgerFailure
