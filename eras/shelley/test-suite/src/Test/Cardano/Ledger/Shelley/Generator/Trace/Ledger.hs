{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger where

import Cardano.Ledger.BaseTypes (Globals, TxIx, mkTxIxPartial)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState,
    LedgerState (..),
    UTxOState,
    genesisState,
  )
import Cardano.Ledger.Shelley.Rules
  ( DelegsEnv,
    DelplEnv,
    LedgerEnv (..),
    ShelleyDELPL,
    ShelleyDelplPredFailure,
    ShelleyLEDGER,
    ShelleyLEDGERS,
    ShelleyLedgersEnv (..),
    UtxoEnv,
  )
import Cardano.Ledger.Shelley.TxBody (DCert)
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import Data.Default.Class (Default)
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Stack
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Constants (Constants (..))
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), genCoin)
import Test.Cardano.Ledger.Shelley.Generator.EraGen
  ( EraGen (..),
    MinLEDGER_STS,
    genUtxo0,
  )
import Test.Cardano.Ledger.Shelley.Generator.Presets (genesisDelegs0)
import Test.Cardano.Ledger.Shelley.Generator.Trace.DCert (CERTS)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)
import Test.Cardano.Ledger.Shelley.Utils
  ( applySTSTest,
    runShelleyBase, ShelleyTest
  )
import Test.QuickCheck (Gen)

-- ======================================================

genAccountState :: Constants -> Gen AccountState
genAccountState Constants {minTreasury, maxTreasury, minReserves, maxReserves} =
  AccountState
    <$> genCoin minTreasury maxTreasury
    <*> genCoin minReserves maxReserves

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance
  ( EraGen era,
    Mock (EraCrypto era),
    MinLEDGER_STS era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (EraCrypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (EraCrypto era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ ShelleyDelplPredFailure era,
    Embed (Core.EraRule "DELEGS" era) (ShelleyLEDGER era),
    Embed (Core.EraRule "UTXOW" era) (ShelleyLEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Core.Tx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (EraCrypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era)),
    Show (State (Core.EraRule "PPUP" era)),
    ShelleyTest era
  ) =>
  TQC.HasTrace (ShelleyLEDGER era) (GenEnv era)
  where
  envGen GenEnv {geConstants} =
    LedgerEnv (SlotNo 0) minBound
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  sigGen genenv env state = genTx genenv env state

  shrinkSignal _ = [] -- TODO add some kind of Shrinker?

  type BaseEnv (ShelleyLEDGER era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

instance
  forall era.
  ( EraGen era,
    Mock (EraCrypto era),
    MinLEDGER_STS era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (EraCrypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (EraCrypto era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ ShelleyDelplPredFailure era,
    Embed (Core.EraRule "DELEG" era) (ShelleyDELPL era),
    Embed (Core.EraRule "LEDGER" era) (ShelleyLEDGERS era),
    ShelleyTest era
  ) =>
  TQC.HasTrace (ShelleyLEDGERS era) (GenEnv era)
  where
  envGen GenEnv {geConstants} =
    LedgersEnv (SlotNo 0)
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  -- a LEDGERS signal is a sequence of LEDGER signals
  sigGen
    ge@(GenEnv _ _ Constants {maxTxsPerBlock})
    (LedgersEnv slotNo pParams reserves)
    ls = do
      (_, txs') <-
        foldM
          genAndApplyTx
          (ls, [])
          [minBound .. mkTxIxPartial (toInteger maxTxsPerBlock - 1)]

      pure $ Seq.fromList (reverse txs') -- reverse Newest first to Oldest first
      where
        genAndApplyTx ::
          HasCallStack =>
          (LedgerState era, [Core.Tx era]) ->
          TxIx ->
          Gen (LedgerState era, [Core.Tx era])
        genAndApplyTx (ls', txs) txIx = do
          let ledgerEnv = LedgerEnv slotNo txIx pParams reserves
          tx <- genTx ge ledgerEnv ls'

          let res =
                runShelleyBase $
                  applySTSTest @(Core.EraRule "LEDGER" era)
                    (TRC (ledgerEnv, ls', tx))
          case res of
            Left pf -> error ("LEDGER sigGen: " <> show pf)
            Right ls'' -> pure (ls'', tx : txs)

  shrinkSignal = const []

  type BaseEnv (ShelleyLEDGERS era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate initial state for the LEDGER STS using the STS environment.
--
-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC LEDGER' (the "initial rule context") instead of simply 'LedgerEnv'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisLedgerState ::
  forall a era ledger.
  (EraGen era, Default (State (Core.EraRule "PPUP" era))) =>
  GenEnv era ->
  IRC ledger ->
  Gen (Either a (LedgerState era))
mkGenesisLedgerState ge@(GenEnv _ _ c) _ =
  Right . genesisState (genesisDelegs0 c) <$> genUtxo0 ge
