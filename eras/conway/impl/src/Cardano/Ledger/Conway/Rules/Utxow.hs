{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxow (
  ConwayUTXOW,
  conwayWitsVKeyNeeded,
)
where

import Cardano.Crypto.DSIGN.Class (Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent,
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent (WrappedShelleyEraEvent),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoEraUTxO, AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUTXO,
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure (..),
  babbageUtxowTransition,
 )
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra, ConwayUTXOW)
import Cardano.Ledger.Conway.Rules.Utxo ()
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.UTxO (getConwayWitsVKeyNeeded)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyUtxoPredFailure,
  ShelleyUtxowEvent (UtxoEvent),
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
 )
import Cardano.Ledger.UTxO (EraUTxO (..), UTxO)
import Control.State.Transition.Extended (Embed (..), STS (..))
import Data.Set (Set)

conwayWitsVKeyNeeded ::
  (EraTx era, ConwayEraTxBody era) =>
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
conwayWitsVKeyNeeded = getConwayWitsVKeyNeeded
{-# DEPRECATED conwayWitsVKeyNeeded "In favor of `getConwayWitsVKeyNeeded` or `getWitsVKeyNeeded`" #-}

-- ================================

type instance EraRuleFailure "UTXOW" (ConwayEra c) = BabbageUtxowPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXOW" BabbageUtxowPredFailure (ConwayEra c)

instance InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxowPredFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure

instance InjectRuleFailure "UTXOW" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance InjectRuleFailure "UTXOW" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = UtxoFailure . injectFailure

instance
  forall era.
  ( AlonzoEraTx era
  , AlonzoEraUTxO era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , ConwayEraTxBody era
  , Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , EraRule "UTXOW" era ~ ConwayUTXOW era
  , InjectRuleFailure "UTXOW" ShelleyUtxowPredFailure era
  , InjectRuleFailure "UTXOW" AlonzoUtxowPredFailure era
  , InjectRuleFailure "UTXOW" BabbageUtxowPredFailure era
  , -- Allow UTXOW to call UTXO
    Embed (EraRule "UTXO" era) (ConwayUTXOW era)
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , Eq (PredicateFailure (EraRule "UTXOS" era))
  , Show (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (ConwayUTXOW era)
  where
  type State (ConwayUTXOW era) = UTxOState era
  type Signal (ConwayUTXOW era) = Tx era
  type Environment (ConwayUTXOW era) = UtxoEnv era
  type BaseM (ConwayUTXOW era) = ShelleyBase
  type PredicateFailure (ConwayUTXOW era) = BabbageUtxowPredFailure era
  type Event (ConwayUTXOW era) = AlonzoUtxowEvent era
  transitionRules = [babbageUtxowTransition @era]
  initialRules = []

instance
  ( Era era
  , STS (BabbageUTXO era)
  , PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era
  , Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era
  , BaseM (ConwayUTXOW era) ~ ShelleyBase
  , PredicateFailure (ConwayUTXOW era) ~ BabbageUtxowPredFailure era
  , Event (ConwayUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabbageUTXO era) (ConwayUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent
