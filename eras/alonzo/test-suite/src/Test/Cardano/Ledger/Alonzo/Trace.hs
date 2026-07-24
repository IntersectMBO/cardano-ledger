{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- HasTrace instances for AlonzoLEDGE
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Export HasTrace instance for AlonzoLEDGE Alonzo Era.
module Test.Cardano.Ledger.Alonzo.Trace () where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (LEDGER, LEDGERS)
import Cardano.Ledger.BaseTypes (Globals, epochInfo, systemStart)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..))
import Cardano.Ledger.Shelley.LedgerState (UTxOState, lsUTxOState, utxosUtxo)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State
import Cardano.Protocol.Crypto (Crypto)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq)
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..), MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (
  genAccountState,
  ledgersSigGen,
 )
import Test.Cardano.Ledger.Shelley.Generator.Trace.TxCert (CERTS)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as TQC

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance
  ( ApplyTx era
  , EraGen era
  , EraGov era
  , EraUTxO era
  , AlonzoEraTx era
  , ShelleyEraAccounts era
  , MinLEDGER_STS era
  , Embed (EraRule "DELPL" era) (CERTS era)
  , Environment (EraRule "DELPL" era) ~ Shelley.DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , PredicateFailure (EraRule "DELPL" era) ~ Shelley.ShelleyDelplPredFailure era
  , Embed (EraRule "DELEGS" era) (LEDGER era)
  , Embed (EraRule "UTXOW" era) (LEDGER era)
  , Environment (EraRule "UTXOW" era) ~ Shelley.UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , Environment (EraRule "DELEGS" era) ~ Shelley.DelegsEnv era
  , State (EraRule "DELEGS" era) ~ CertState era
  , Signal (EraRule "DELEGS" era) ~ Seq (TxCert era)
  , AtMostEra "Babbage" era
  , EraCertState era
  , Crypto c
  , EraRuleFailure "LEDGER" era ~ Shelley.ShelleyLedgerPredFailure era
  , EraRule "LEDGER" era ~ LEDGER era
  ) =>
  TQC.HasTrace (LEDGER era) (GenEnv c era)
  where
  envGen GenEnv {geConstants} =
    Shelley.LedgerEnv (SlotNo 0) Nothing minBound
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  sigGen ge ledgerEnv@(Shelley.LedgerEnv _ _ _ pParams _) ls = do
    tx <- genTx ge ledgerEnv ls
    pure $
      mkStAnnTx
        (epochInfo testGlobals)
        (systemStart testGlobals)
        pParams
        (utxosUtxo (lsUTxOState ls))
        mempty
        tx

  shrinkSignal _ = [] -- TODO add some kind of Shrinker?

  type BaseEnv (LEDGER era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

instance
  ( Crypto c
  , ApplyTx era
  , EraGen era
  , EraGov era
  , EraUTxO era
  , EraStake era
  , EraCertState era
  , ShelleyEraAccounts era
  , MinLEDGER_STS era
  , Embed (EraRule "DELPL" era) (CERTS era)
  , Environment (EraRule "DELPL" era) ~ Shelley.DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , PredicateFailure (EraRule "DELPL" era) ~ Shelley.ShelleyDelplPredFailure era
  , Embed (EraRule "DELEG" era) (Shelley.DELPL era)
  , Embed (EraRule "LEDGER" era) (LEDGERS era)
  , AtMostEra "Babbage" era
  ) =>
  TQC.HasTrace (LEDGERS era) (GenEnv c era)
  where
  envGen GenEnv {geConstants} =
    Shelley.LedgersEnv (SlotNo 0) (EpochNo 0)
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  sigGen = ledgersSigGen
  shrinkSignal = const []

  type BaseEnv (LEDGERS era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals
