{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.Types
  ( TotalAda (..),
    getTxOutCoin,
  )
where

import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.API (AccountState (AccountState), Coin, DPState (DPState), DState, EpochState (esAccountState, esLState), LedgerState (LedgerState), NewEpochState (nesEs), UTxO (UTxO), UTxOState (UTxOState))
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Ledger.Shelley.LedgerState (_unified)
import qualified Cardano.Ledger.UnifiedMap as UMap
import Cardano.Ledger.Val (Val (coin, (<+>)))
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Generic.Proof (Proof (..), Reflect (reify))

getTxOutCoin :: Proof era -> Core.TxOut era -> Coin
getTxOutCoin (Babbage _) (Babbage.TxOut _ v _ _) = coin v
getTxOutCoin (Alonzo _) (TxOut _ v _) = coin v
getTxOutCoin (Mary _) (Shelley.TxOut _ v) = coin v
getTxOutCoin (Allegra _) (Shelley.TxOut _ v) = coin v
getTxOutCoin (Shelley _) (Shelley.TxOut _ v) = coin v

-- | Compute the total Ada from Ada pots within 't'
class TotalAda t where
  totalAda :: t -> Coin

instance TotalAda AccountState where
  totalAda (AccountState treasury reserves) = treasury <+> reserves

instance Reflect era => TotalAda (UTxOState era) where
  totalAda (UTxOState utxo deposits fees _ _) = totalAda utxo <+> deposits <+> fees

instance TotalAda (DState era) where
  totalAda dstate = Fold.foldl' (<+>) mempty (UMap.Rewards (_unified dstate))

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
      accum ans txout = getTxOutCoin reify txout <+> ans
