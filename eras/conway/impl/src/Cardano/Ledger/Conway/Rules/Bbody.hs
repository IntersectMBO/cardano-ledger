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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Bbody (
  ConwayBBODY,
  ConwayBbodyPredFailure (..),
  alonzoToConwayBbodyPredFailure,
  shelleyToConwayBbodyPredFailure,
  totalRefScriptSizeInBlock,
) where

import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.BlockBody (AlonzoBlockBody)
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
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, IsValid (..), isValidTxL)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import Cardano.Ledger.Babbage.Collateral (collOuts)
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  ProtVer,
  Relation (..),
  ShelleyBase,
  natVersion,
  pvMajor,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway.Era (ConwayBBODY, ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules.Cert (ConwayCertPredFailure)
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Ledger (ConwayLedgerPredFailure)
import Cardano.Ledger.Conway.Rules.Ledgers ()
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
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
import Cardano.Ledger.Shelley.UTxO (UTxO (..), txouts, unUTxO)
import Control.State.Transition (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  (?!),
 )
import Data.Foldable (Foldable (foldMap'))
import qualified Data.Foldable as F (foldl')
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (getSum))
import qualified Data.Monoid as Monoid (Sum (..))
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ConwayBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch 'RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch 'RelEQ (Hash HASH EraIndependentBlockBody))
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure (PredicateFailure (EraRule "LEDGERS" era))
  | TooManyExUnits (Mismatch 'RelLTEQ ExUnits)
  | BodyRefScriptsSizeTooBig (Mismatch 'RelLTEQ Int)
  deriving (Generic)

deriving instance
  (Era era, Show (PredicateFailure (EraRule "LEDGERS" era))) =>
  Show (ConwayBbodyPredFailure era)

deriving instance
  (Era era, Eq (PredicateFailure (EraRule "LEDGERS" era))) =>
  Eq (ConwayBbodyPredFailure era)

deriving anyclass instance
  (Era era, NoThunks (PredicateFailure (EraRule "LEDGERS" era))) =>
  NoThunks (ConwayBbodyPredFailure era)

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
      TooManyExUnits mm -> Sum TooManyExUnits 3 !> ToGroup mm
      BodyRefScriptsSizeTooBig mm -> Sum BodyRefScriptsSizeTooBig 4 !> ToGroup mm

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
    3 -> SumD TooManyExUnits <! FromGroup
    4 -> SumD BodyRefScriptsSizeTooBig <! FromGroup
    n -> Invalid n

type instance EraRuleFailure "BBODY" ConwayEra = ConwayBbodyPredFailure ConwayEra

type instance EraRuleEvent "BBODY" ConwayEra = AlonzoBbodyEvent ConwayEra

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure ConwayEra

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure ConwayEra where
  injectFailure = alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertsPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovCertPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovPredFailure ConwayEra where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

shelleyToConwayBbodyPredFailure ::
  forall era.
  ShelleyBbodyPredFailure era ->
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
  AlonzoBbodyPredFailure era ->
  ConwayBbodyPredFailure era
alonzoToConwayBbodyPredFailure (ShelleyInAlonzoBbodyPredFailure x) = shelleyToConwayBbodyPredFailure x
alonzoToConwayBbodyPredFailure (Alonzo.TooManyExUnits m) = TooManyExUnits m

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , AlonzoEraTxWits era
  , BlockBody era ~ AlonzoBlockBody era
  , EraBlockBody era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , EraRule "BBODY" era ~ ConwayBBODY era
  , AlonzoEraTx era
  , BabbageEraTxBody era
  , ConwayEraPParams era
  ) =>
  STS (ConwayBBODY era)
  where
  type State (ConwayBBODY era) = ShelleyBbodyState era

  type Signal (ConwayBBODY era) = Block BHeaderView era

  type Environment (ConwayBBODY era) = BbodyEnv era

  type BaseM (ConwayBBODY era) = ShelleyBase

  type PredicateFailure (ConwayBBODY era) = ConwayBbodyPredFailure era

  type Event (ConwayBBODY era) = AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [conwayBbodyTransition @era >> alonzoBbodyTransition @era]

conwayBbodyTransition ::
  forall era.
  ( Signal (EraRule "BBODY" era) ~ Block BHeaderView era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , BlockBody era ~ AlonzoBlockBody era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , AlonzoEraTx era
  , EraBlockBody era
  , BabbageEraTxBody era
  , ConwayEraPParams era
  ) =>
  TransitionRule (EraRule "BBODY" era)
conwayBbodyTransition = do
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp _
               , state@(BbodyState ls _)
               , Block _ txsSeq
               )
           ) -> do
        let utxo = utxosUtxo (lsUTxOState ls)
            txs = txsSeq ^. txSeqBlockBodyL
            totalRefScriptSize = totalRefScriptSizeInBlock (pp ^. ppProtocolVersionL) txs utxo
            maxRefScriptSizePerBlock = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerBlockG
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
  Embed ledgers (ConwayBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent

totalRefScriptSizeInBlock ::
  (AlonzoEraTx era, BabbageEraTxBody era) => ProtVer -> StrictSeq (Tx era) -> UTxO era -> Int
totalRefScriptSizeInBlock protVer txs (UTxO utxo)
  | pvMajor protVer <= natVersion @10 =
      getSum $ foldMap' (Monoid.Sum . txNonDistinctRefScriptsSize (UTxO utxo)) txs
  | otherwise =
      snd $ F.foldl' accum (utxo, 0) txs
  where
    accum (!accUtxo, !accSum) tx =
      let updatedUtxo = accUtxo `Map.union` unUTxO toAdd
          toAdd
            | IsValid True <- tx ^. isValidTxL = txouts $ tx ^. bodyTxL
            | otherwise = collOuts $ tx ^. bodyTxL
       in (updatedUtxo, accSum + txNonDistinctRefScriptsSize (UTxO accUtxo) tx)
