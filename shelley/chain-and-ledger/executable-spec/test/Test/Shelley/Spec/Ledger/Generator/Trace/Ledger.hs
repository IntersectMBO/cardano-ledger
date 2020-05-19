{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Ledger where

import Control.Monad (foldM)
import Control.Monad.Trans.Reader (runReaderT)
import Control.State.Transition.Extended (IRC, TRC (..), applySTS)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import Data.Functor.Identity (runIdentity)
import qualified Data.Sequence as Seq
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.BaseTypes (Globals)
import Shelley.Spec.Ledger.LedgerState (AccountState (..), genesisState, pattern LedgerState)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import Shelley.Spec.Ledger.STS.Ledgers (LedgersEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.TxData (Ix)
import Test.QuickCheck (Gen)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( DPState,
    LEDGER,
    LEDGERS,
    Tx,
    UTxOState,
  )
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), genCoin)
import Test.Shelley.Spec.Ledger.Generator.Presets (genUtxo0, genesisDelegs0)
import Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import Test.Shelley.Spec.Ledger.Shrinkers (shrinkTx)
import Test.Shelley.Spec.Ledger.Utils (runShelleyBase)

genAccountState :: HasCallStack => Constants -> Gen AccountState
genAccountState (Constants {minTreasury, maxTreasury, minReserves, maxReserves}) =
  AccountState
    <$> genCoin minTreasury maxTreasury
    <*> genCoin minReserves maxReserves

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance TQC.HasTrace LEDGER GenEnv where
  envGen GenEnv {geConstants} =
    LedgerEnv <$> pure (SlotNo 0)
      <*> pure 0
      <*> genPParams geConstants
      <*> genAccountState geConstants

  sigGen = genTx

  shrinkSignal = shrinkTx

  type BaseEnv LEDGER = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

instance TQC.HasTrace LEDGERS GenEnv where
  envGen GenEnv {geConstants} =
    LedgersEnv <$> pure (SlotNo 0)
      <*> genPParams geConstants
      <*> genAccountState geConstants

  -- a LEDGERS signal is a sequence of LEDGER signals
  sigGen
    ge@(GenEnv _ Constants {maxTxsPerBlock})
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
          (UTxOState, DPState, [Tx]) ->
          Ix ->
          Gen (UTxOState, DPState, [Tx])
        genAndApplyTx (u, dp, txs) ix = do
          let ledgerEnv = LedgerEnv slotNo ix pParams reserves
          tx <- genTx ge ledgerEnv (u, dp)

          let res = runShelleyBase $ applySTS @LEDGER (TRC (ledgerEnv, (u, dp), tx))
          pure $ case res of
            Left _ ->
              (u, dp, txs)
            Right (u', dp') ->
              (u', dp', tx : txs)

  shrinkSignal = const []

  type BaseEnv LEDGERS = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate initial state for the LEDGER STS using the STS environment.
--
-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC LEDGER' (the "initial rule context") instead of simply 'LedgerEnv'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisLedgerState ::
  Constants ->
  IRC LEDGER ->
  Gen (Either a (UTxOState, DPState))
mkGenesisLedgerState c _ = do
  utxo0 <- genUtxo0 c
  let (LedgerState utxoSt dpSt) = genesisState (genesisDelegs0 c) utxo0
  pure $ Right (utxoSt, dpSt)
