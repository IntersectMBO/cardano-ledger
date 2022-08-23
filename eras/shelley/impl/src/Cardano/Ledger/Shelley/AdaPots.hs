{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.AdaPots
  ( AdaPots (..),
    totalAdaES,
    totalAdaPotsES,
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    EpochState (..),
    LedgerState (..),
    UTxOState (..),
    rewards,
  )
import Cardano.Ledger.Shelley.UTxO (coinBalance)
import Data.Foldable (fold)

data AdaPots = AdaPots
  { treasuryAdaPot :: Coin,
    reservesAdaPot :: Coin,
    rewardsAdaPot :: Coin,
    utxoAdaPot :: Coin,
    depositsAdaPot :: Coin,
    feesAdaPot :: Coin
  }
  deriving (Show, Eq)

-- | Calculate the total ada pots in the epoch state
totalAdaPotsES ::
  EraTxOut era =>
  EpochState era ->
  AdaPots
totalAdaPotsES (EpochState (AccountState treasury_ reserves_) _ ls _ _ _) =
  AdaPots
    { treasuryAdaPot = treasury_,
      reservesAdaPot = reserves_,
      rewardsAdaPot = rewards_,
      utxoAdaPot = coins,
      depositsAdaPot = deposits,
      feesAdaPot = fees_
    }
  where
    UTxOState u deposits fees_ _ _ = lsUTxOState ls
    DPState dstate _ = lsDPState ls
    rewards_ = fold (rewards dstate)
    coins = coinBalance u

-- | Calculate the total ada in the epoch state
totalAdaES :: EraTxOut era => EpochState era -> Coin
totalAdaES cs =
  treasuryAdaPot
    <> reservesAdaPot
    <> rewardsAdaPot
    <> utxoAdaPot
    <> depositsAdaPot
    <> feesAdaPot
  where
    AdaPots
      { treasuryAdaPot,
        reservesAdaPot,
        rewardsAdaPot,
        utxoAdaPot,
        depositsAdaPot,
        feesAdaPot
      } = totalAdaPotsES cs
