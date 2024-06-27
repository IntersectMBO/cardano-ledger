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
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway.Era (ConwayBBODY, ConwayEra)
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
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), utxosUtxo)
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

data ConwayBbodyPredFailure era
  = WrongBlockBodySizeBBODY
      -- | Actual Body Size
      !Int
      -- | Claimed Body Size in Header
      !Int
  | InvalidBodyHashBBODY
      -- | Actual Hash
      !(Hash (EraCrypto era) EraIndependentBlockBody)
      -- | Claimed Hash
      !(Hash (EraCrypto era) EraIndependentBlockBody)
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure !(PredicateFailure (EraRule "LEDGERS" era))
  | TooManyExUnits
      -- | Computed Sum of ExUnits for all plutus scripts
      !ExUnits
      -- | Maximum allowed by protocal parameters
      !ExUnits
  | RefScriptsSizeTooBig
      -- | Computed sum of reference script size
      Int
      -- | Maximum allowed total reference script size
      Int
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
      WrongBlockBodySizeBBODY x y -> Sum WrongBlockBodySizeBBODY 0 !> To x !> To y
      InvalidBodyHashBBODY x y -> Sum (InvalidBodyHashBBODY @era) 1 !> To x !> To y
      LedgersFailure x -> Sum (LedgersFailure @era) 2 !> To x
      TooManyExUnits x y -> Sum TooManyExUnits 3 !> To x !> To y
      RefScriptsSizeTooBig x y -> Sum RefScriptsSizeTooBig 4 !> To x !> To y

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (ConwayBbodyPredFailure era)
  where
  decCBOR = decode . Summands "ConwayBbodyPred" $ \case
    0 -> SumD WrongBlockBodySizeBBODY <! From <! From
    1 -> SumD InvalidBodyHashBBODY <! From <! From
    2 -> SumD LedgersFailure <! From
    3 -> SumD TooManyExUnits <! From <! From
    4 -> SumD RefScriptsSizeTooBig <! From <! From
    n -> Invalid n

type instance EraRuleFailure "BBODY" (ConwayEra c) = ConwayBbodyPredFailure (ConwayEra c)

type instance EraRuleEvent "BBODY" (ConwayEra c) = AlonzoBbodyEvent (ConwayEra c)

instance InjectRuleFailure "BBODY" ConwayBbodyPredFailure (ConwayEra c)

instance InjectRuleFailure "BBODY" AlonzoBbodyPredFailure (ConwayEra c) where
  injectFailure = alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" ConwayLedgerPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertsPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ConwayGovPredFailure (ConwayEra c) where
  injectFailure = shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure . injectFailure

shelleyToConwayBbodyPredFailure ::
  forall era.
  ShelleyBbodyPredFailure era ->
  ConwayBbodyPredFailure era
shelleyToConwayBbodyPredFailure (Shelley.WrongBlockBodySizeBBODY x y) = WrongBlockBodySizeBBODY x y
shelleyToConwayBbodyPredFailure (Shelley.InvalidBodyHashBBODY x y) = InvalidBodyHashBBODY x y
shelleyToConwayBbodyPredFailure (Shelley.LedgersFailure x) = LedgersFailure x

alonzoToConwayBbodyPredFailure ::
  forall era.
  AlonzoBbodyPredFailure era ->
  ConwayBbodyPredFailure era
alonzoToConwayBbodyPredFailure (ShelleyInAlonzoBbodyPredFailure x) = shelleyToConwayBbodyPredFailure x
alonzoToConwayBbodyPredFailure (Alonzo.TooManyExUnits x y) = TooManyExUnits x y

instance
  ( DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (AlonzoTx era)
  , AlonzoEraTxWits era
  , Tx era ~ AlonzoTx era
  , Era.TxSeq era ~ AlonzoTxSeq era
  , Tx era ~ AlonzoTx era
  , EraSegWits era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  , EraRule "BBODY" era ~ ConwayBBODY era
  ) =>
  STS (ConwayBBODY era)
  where
  type
    State (ConwayBBODY era) =
      ShelleyBbodyState era

  type
    Signal (ConwayBBODY era) =
      Block (BHeaderView (EraCrypto era)) era

  type Environment (ConwayBBODY era) = BbodyEnv era

  type BaseM (ConwayBBODY era) = ShelleyBase

  type PredicateFailure (ConwayBBODY era) = ConwayBbodyPredFailure era
  type Event (ConwayBBODY era) = AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [conwayBbodyTransition @era]

conwayBbodyTransition ::
  forall era.
  ( STS (EraRule "BBODY" era)
  , Signal (EraRule "BBODY" era) ~ Block (BHeaderView (EraCrypto era)) era
  , State (EraRule "BBODY" era) ~ ShelleyBbodyState era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , EraSegWits era
  , AlonzoEraTxWits era
  , Era.TxSeq era ~ AlonzoTxSeq era
  , Tx era ~ AlonzoTx era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" AlonzoBbodyPredFailure era
  ) =>
  TransitionRule (EraRule "BBODY" era)
conwayBbodyTransition = alonzoBbodyTransition @era

instance
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Era era
  ) =>
  Embed ledgers (ConwayBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = ShelleyInAlonzoEvent . LedgersEvent
