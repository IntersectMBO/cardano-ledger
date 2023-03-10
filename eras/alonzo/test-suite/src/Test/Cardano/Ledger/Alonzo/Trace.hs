{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- HasTrace instances for AlonzoLEDGE
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Export HasTrace instance for AlonzoLEDGE Alonzo Era.
module Test.Cardano.Ledger.Alonzo.Trace () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, AlonzoTx)
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Shelley.LedgerState (CertState (..), UTxOState)
import Cardano.Ledger.Shelley.Rules (
  DelegsEnv,
  DelplEnv,
  LedgerEnv (..),
  ShelleyDelplPredFailure,
  UtxoEnv,
 )
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq)
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.DCert (CERTS)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (genAccountState)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)

-- The AlonzoLEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance
  ( EraGen era
  , AlonzoEraTx era
  , Mock (EraCrypto era)
  , MinLEDGER_STS era
  , Embed (EraRule "DELPL" era) (CERTS era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ DCert era
  , PredicateFailure (EraRule "DELPL" era) ~ ShelleyDelplPredFailure era
  , Embed (EraRule "DELEGS" era) (AlonzoLEDGER era)
  , Embed (EraRule "UTXOW" era) (AlonzoLEDGER era)
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert era)
  , Tx era ~ AlonzoTx era
  , ProtVerAtMost era 8
  ) =>
  TQC.HasTrace (AlonzoLEDGER era) (GenEnv era)
  where
  envGen GenEnv {geConstants} =
    LedgerEnv (SlotNo 0) minBound
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  sigGen genenv env state = genTx genenv env state

  shrinkSignal _ = [] -- TODO add some kind of Shrinker?

  type BaseEnv (AlonzoLEDGER era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
