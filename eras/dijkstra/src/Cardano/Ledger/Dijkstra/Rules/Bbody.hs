{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
  maxRefScriptSizePerBlock,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoBbodyEvent (..),
  AlonzoBbodyPredFailure (ShelleyInAlonzoBbodyPredFailure),
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
  alonzoBbodyTransition,
 )
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo (AlonzoBbodyPredFailure (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq, txSeqTxns)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (Mismatch (..), Relation (..), ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraBBODY, DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules.Cert (DijkstraCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Certs (DijkstraCertsPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Deleg (DijkstraDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxos (DijkstraUtxosPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Dijkstra.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), lsUTxOState, utxosUtxo)
import Cardano.Ledger.Shelley.Rules (
  BbodyEnv (..),
  ShelleyBbodyEvent (..),
  ShelleyBbodyPredFailure,
  ShelleyBbodyState (..),
  ShelleyLedgersEnv (..),
  ShelleyLedgersPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley (ShelleyBbodyPredFailure (..))
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  (?!),
 )
import Data.Foldable (Foldable (foldMap'))
import Data.Monoid (Sum (getSum))
import qualified Data.Monoid as Monoid (Sum (..))
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | In the next era this will become a proper protocol parameter.
-- For now this is a hard coded limit on the total number of bytes of all reference scripts
-- combined from all transactions within a block.
maxRefScriptSizePerBlock :: Int
maxRefScriptSizePerBlock = 1024 * 1024 -- 1MiB

data DijkstraBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch 'RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch 'RelEQ (Hash HASH EraIndependentBlockBody))
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure (PredicateFailure (EraRule "LEDGERS" era))
  | TooManyExUnits (Mismatch 'RelLTEQ ExUnits)
  | BodyRefScriptsSizeTooBig (Mismatch 'RelLTEQ Int)
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
      WrongBlockBodySizeBBODY mm -> Sum WrongBlockBodySizeBBODY 0 !> ToGroup mm
      InvalidBodyHashBBODY mm -> Sum (InvalidBodyHashBBODY @era) 1 !> ToGroup mm
      LedgersFailure x -> Sum (LedgersFailure @era) 2 !> To x
      TooManyExUnits mm -> Sum TooManyExUnits 3 !> ToGroup mm
      BodyRefScriptsSizeTooBig mm -> Sum BodyRefScriptsSizeTooBig 4 !> ToGroup mm

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (DijkstraBbodyPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraBbodyPred" $ \case
    0 -> SumD WrongBlockBodySizeBBODY <! FromGroup
    1 -> SumD InvalidBodyHashBBODY <! FromGroup
    2 -> SumD LedgersFailure <! From
    3 -> SumD TooManyExUnits <! FromGroup
    4 -> SumD BodyRefScriptsSizeTooBig <! FromGroup
    n -> Invalid n

type instance EraRuleFailure "BBODY" DijkstraEra = DijkstraBbodyPredFailure DijkstraEra

type instance EraRuleEvent "BBODY" DijkstraEra = AlonzoBbodyEvent DijkstraEra

instance InjectRuleFailure "BBODY" DijkstraBbodyPredFailure DijkstraEra

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure DijkstraEra where
  injectFailure = alonzoToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxosPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraCertsPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraCertPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraDelegPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraBbodyPredFailure . Shelley.LedgersFailure . injectFailure

shelleyToDijkstraBbodyPredFailure ::
  forall era.
  ShelleyBbodyPredFailure era ->
  DijkstraBbodyPredFailure era
shelleyToDijkstraBbodyPredFailure
  (Shelley.WrongBlockBodySizeBBODY m) =
    WrongBlockBodySizeBBODY m
shelleyToDijkstraBbodyPredFailure
  (Shelley.InvalidBodyHashBBODY m) =
    InvalidBodyHashBBODY m
shelleyToDijkstraBbodyPredFailure (Shelley.LedgersFailure x) = LedgersFailure x

alonzoToDijkstraBbodyPredFailure ::
  forall era.
  AlonzoBbodyPredFailure era ->
  DijkstraBbodyPredFailure era
alonzoToDijkstraBbodyPredFailure (ShelleyInAlonzoBbodyPredFailure x) = shelleyToDijkstraBbodyPredFailure x
alonzoToDijkstraBbodyPredFailure (Alonzo.TooManyExUnits m) = TooManyExUnits m

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (AlonzoTx era)
  , AlonzoEraTxWits era
  , Tx era ~ AlonzoTx era
  , TxSeq era ~ AlonzoTxSeq era
  , EraSegWits era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , EraRule "BBODY" era ~ DijkstraBBODY era
  , EraTx era
  , BabbageEraTxBody era
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
  transitionRules = [conwayBbodyTransition @era >> alonzoBbodyTransition @era]

conwayBbodyTransition ::
  forall era.
  ( Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , TxSeq era ~ AlonzoTxSeq era
  , Tx era ~ AlonzoTx era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , EraTx era
  , BabbageEraTxBody era
  ) =>
  TransitionRule (EraRule "BBODY" era)
conwayBbodyTransition = do
  judgmentContext
    >>= \( TRC
            ( _
              , state@(BbodyState ls _)
              , Block _ txsSeq
              )
          ) -> do
        let utxo = utxosUtxo (lsUTxOState ls)
            txs = txSeqTxns txsSeq
            totalRefScriptSize =
              getSum $ foldMap' (Monoid.Sum . txNonDistinctRefScriptsSize utxo) txs
        totalRefScriptSize
          <= maxRefScriptSizePerBlock
            ?! injectFailure
              ( BodyRefScriptsSizeTooBig $
                  Mismatch
                    { mismatchSupplied = totalRefScriptSize
                    , mismatchExpected = maxRefScriptSizePerBlock
                    }
              )
        pure state

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  ) =>
  Embed ledgers (DijkstraBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent
