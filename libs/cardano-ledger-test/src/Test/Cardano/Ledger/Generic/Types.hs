{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Types
  ( TotalAda (..),
  )
where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API
  ( AccountState (AccountState),
    Coin,
    DPState (DPState),
    DState,
    EpochState (esAccountState, esLState),
    LedgerState (LedgerState),
    NewEpochState (nesEs),
    UTxO (UTxO),
    UTxOState (UTxOState),
  )
import Cardano.Ledger.Shelley.LedgerState (dsUnified)
import qualified Cardano.Ledger.UMapCompact as UM
import Cardano.Ledger.Val (Val ((<+>)))
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Generic.Proof (Reflect)

-- | Compute the total Ada from Ada pots within 't'
class TotalAda t where
  totalAda :: t -> Coin

instance TotalAda AccountState where
  totalAda (AccountState treasury reserves) = treasury <+> reserves

instance Reflect era => TotalAda (UTxOState era) where
  totalAda (UTxOState utxo deposits fees _ _) = totalAda utxo <+> deposits <+> fees

instance TotalAda (DState era) where
  totalAda dstate = UM.fromCompact $ Fold.foldl' UM.addCompact mempty (UM.Rewards (dsUnified dstate))

instance TotalAda (DPState era) where
  totalAda (DPState ds _) = totalAda ds

instance Reflect era => TotalAda (LedgerState era) where
  totalAda (LedgerState utxos dps) = totalAda utxos <+> totalAda dps

instance Reflect era => TotalAda (EpochState era) where
  totalAda eps = totalAda (esLState eps) <+> totalAda (esAccountState eps)

instance Reflect era => TotalAda (NewEpochState era) where
  totalAda nes = totalAda (nesEs nes)

instance Reflect era => TotalAda (UTxO era) where
  totalAda (UTxO m) = Map.foldl' accum mempty m
    where
      accum ans txOut = txOut ^. coinTxOutL <+> ans
