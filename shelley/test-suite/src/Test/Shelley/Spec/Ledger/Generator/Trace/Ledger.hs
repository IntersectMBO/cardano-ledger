{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Ledger where

import Cardano.Binary (ToCBOR)
import Cardano.Ledger.BaseTypes (Globals)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.Constraints
  ( TransValue,
    UsesAuxiliary,
    UsesTxBody,
    UsesTxOut,
    UsesValue,
  )
import Cardano.Ledger.Slot (SlotNo (..))
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import Data.Default.Class (Default)
import Data.Functor.Identity (runIdentity)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Records (HasField)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState,
    LedgerState (..),
    UTxOState,
    genesisState,
  )
import Shelley.Spec.Ledger.STS.Delegs (DelegsEnv)
import Shelley.Spec.Ledger.STS.Delpl (DELPL, DelplEnv, DelplPredicateFailure)
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv (..))
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersEnv (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv)
import Shelley.Spec.Ledger.TxBody (DCert, Ix, TxIn)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), genCoin)
import Test.Shelley.Spec.Ledger.Generator.EraGen
  ( EraGen (..),
    MinLEDGER_STS,
    genUtxo0,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (genesisDelegs0)
import Test.Shelley.Spec.Ledger.Generator.Trace.DCert (CERTS)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import Test.Shelley.Spec.Ledger.Utils
  ( applySTSTest,
    runShelleyBase,
  )

-- ======================================================

genAccountState :: Constants -> Gen AccountState
genAccountState (Constants {minTreasury, maxTreasury, minReserves, maxReserves}) =
  AccountState
    <$> genCoin minTreasury maxTreasury
    <*> genCoin minReserves maxReserves

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance
  ( EraGen era,
    UsesTxBody era,
    UsesTxOut era,
    UsesValue era,
    UsesAuxiliary era,
    Mock (Crypto era),
    MinLEDGER_STS era,
    TransValue ToCBOR era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ DelplPredicateFailure era,
    Embed (Core.EraRule "DELEGS" era) (LEDGER era),
    Embed (Core.EraRule "UTXOW" era) (LEDGER era),
    Environment (Core.EraRule "UTXOW" era) ~ UtxoEnv era,
    State (Core.EraRule "UTXOW" era) ~ UTxOState era,
    Signal (Core.EraRule "UTXOW" era) ~ Core.Tx era,
    Environment (Core.EraRule "DELEGS" era) ~ DelegsEnv era,
    State (Core.EraRule "DELEGS" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELEGS" era) ~ Seq (DCert (Crypto era)),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    Show (State (Core.EraRule "PPUP" era)),
    Show (Core.Tx era)
  ) =>
  TQC.HasTrace (LEDGER era) (GenEnv era)
  where
  envGen GenEnv {geConstants} =
    LedgerEnv (SlotNo 0) 0
      <$> genEraPParams @era geConstants
      <*> genAccountState geConstants

  sigGen genenv env state = genTx genenv env state

  shrinkSignal _ = [] -- TODO add some kind of Shrinker?

  type BaseEnv (LEDGER era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

instance
  forall era.
  ( EraGen era,
    UsesTxBody era,
    UsesTxOut era,
    UsesValue era,
    UsesAuxiliary era,
    Mock (Crypto era),
    MinLEDGER_STS era,
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era),
    PredicateFailure (Core.EraRule "DELPL" era) ~ DelplPredicateFailure era,
    Embed (Core.EraRule "DELEG" era) (DELPL era),
    Embed (Core.EraRule "LEDGER" era) (LEDGERS era),
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era)),
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  TQC.HasTrace (LEDGERS era) (GenEnv era)
  where
  envGen GenEnv {geConstants} =
    LedgersEnv <$> pure (SlotNo 0)
      <*> genEraPParams @era geConstants
      <*> genAccountState geConstants

  -- a LEDGERS signal is a sequence of LEDGER signals
  sigGen
    ge@(GenEnv _ _ Constants {maxTxsPerBlock})
    (LedgersEnv slotNo pParams reserves)
    (LedgerState utxoSt dpSt) = do
      (_, _, txs') <-
        foldM
          genAndApplyTx
          (utxoSt, dpSt, [])
          [0 .. (fromIntegral maxTxsPerBlock - 1)]

      pure $ Seq.fromList (reverse txs') -- reverse Newest first to Oldest first
      where
        genAndApplyTx ::
          (UTxOState era, DPState (Crypto era), [Core.Tx era]) ->
          Ix ->
          Gen (UTxOState era, DPState (Crypto era), [Core.Tx era])
        genAndApplyTx (u, dp, txs) ix = do
          let ledgerEnv = LedgerEnv slotNo ix pParams reserves
          tx <- genTx ge ledgerEnv (u, dp)

          let res =
                runShelleyBase $
                  applySTSTest @(Core.EraRule "LEDGER" era)
                    (TRC (ledgerEnv, (u, dp), tx))
          pure $ case res of
            Left pf -> (error ("LEDGER sigGen: " <> show pf))
            Right (u', dp') ->
              (u', dp', tx : txs)

  shrinkSignal = const []

  type BaseEnv (LEDGERS era) = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate initial state for the LEDGER STS using the STS environment.
--
-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC LEDGER' (the "initial rule context") instead of simply 'LedgerEnv'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisLedgerState ::
  forall a era ledger.
  ( UsesValue era,
    EraGen era,
    Default (State (Core.EraRule "PPUP" era))
  ) =>
  GenEnv era ->
  IRC ledger ->
  Gen (Either a (UTxOState era, DPState (Crypto era)))
mkGenesisLedgerState ge@(GenEnv _ _ c) _ = do
  utxo0 <- genUtxo0 ge
  let (LedgerState utxoSt dpSt) = genesisState (genesisDelegs0 c) utxo0
  pure $ Right (utxoSt, dpSt)
