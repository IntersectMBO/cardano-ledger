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

import Cardano.Ledger.Alonzo.Rules (AlonzoLEDGER)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, AlonzoTx)
import Cardano.Ledger.Alonzo.TxBody ()
import Cardano.Ledger.BaseTypes (Globals)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Shelley.LedgerState (DPState (..), UTxOState)
import Cardano.Ledger.Shelley.Rules
  ( DelegsEnv,
    DelplEnv,
    LedgerEnv (..),
    ShelleyDelplPredFailure,
    UtxoEnv,
  )
import Cardano.Ledger.Shelley.TxBody (DCert)
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
  ( EraGen era,
    AlonzoEraTx era,
    Mock (EraCrypto era),
    MinLEDGER_STS era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (EraCrypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (EraCrypto era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ ShelleyDelplPredFailure era,
    Embed (Core.EraRule "DELEGS" era) (AlonzoLEDGER era),
    Embed (Core.EraRule "UTXOW" era) (AlonzoLEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Core.Tx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (EraCrypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era)),
    Show (State (Core.EraRule "PPUP" era)),
    Core.Tx era ~ AlonzoTx era
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
