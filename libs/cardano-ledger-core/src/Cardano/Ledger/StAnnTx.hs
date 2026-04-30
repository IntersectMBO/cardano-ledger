{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.StAnnTx (
  EraStAnnTx (..),
) where

import Cardano.Ledger.Core (EraTx, PParams, TopTx, Tx, TxLevel)
import Cardano.Ledger.State.UTxO (UTxO)
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Kind (Type)
import Data.Text (Text)
import Lens.Micro (SimpleGetter)

class EraTx era => EraStAnnTx era where
  -- | This is a `Tx` that is annotated with some pre-computed data derived from the ledger state,
  -- which can be used to avoid redundant computation when a transaction is validated multiple
  -- times.
  --
  -- It is critical to only store here information that satisfies the following property: if the
  -- ledger state changes in a way that makes the annotated value stale, then some other predicate
  -- check in the STS rules must independently cause the transaction to be rejected.  Stale
  -- annotations must never lead to a transaction being silently accepted.
  --
  -- For example, if a reference input gets spent, then there must a predicate check that fails on
  -- missing output, regardless if data from reference inputs is still present in the `StAnnTx`
  type StAnnTx (l :: TxLevel) era = (r :: Type) | r -> l era

  txStAnnTxG :: SimpleGetter (StAnnTx l era) (Tx l era)

  mkStAnnTx ::
    EpochInfo (Either Text) ->
    SystemStart ->
    PParams era ->
    UTxO era ->
    Tx TopTx era ->
    StAnnTx TopTx era
