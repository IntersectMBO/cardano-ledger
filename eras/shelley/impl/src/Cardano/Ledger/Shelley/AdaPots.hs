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
    Produced (..),
    Consumed (..),
    consumedTxBody,
    producedTxBody,
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState (..),
    DPState (..),
    DState (..),
    EpochState (..),
    LedgerState (..),
    PState (..),
    UTxOState (..),
    keyTxRefunds,
    rewards,
    totalTxDeposits,
  )
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), unWdrl)
import Cardano.Ledger.UTxO (UTxO (..), coinBalance, txouts)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import GHC.Records (HasField (..))
import Lens.Micro ((^.))

data AdaPots = AdaPots
  { treasuryAdaPot :: Coin,
    reservesAdaPot :: Coin,
    rewardsAdaPot :: Coin,
    utxoAdaPot :: Coin,
    keyDepositAdaPot :: Coin,
    poolDepositAdaPot :: Coin,
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
      keyDepositAdaPot = keyDeposits_,
      poolDepositAdaPot = poolDeposits_,
      depositsAdaPot = deposits,
      feesAdaPot = fees_
    }
  where
    UTxOState u deposits fees_ _ _ = lsUTxOState ls
    DPState dstate _ = lsDPState ls
    rewards_ = fromCompact $ fold (rewards dstate)
    coins = coinBalance u
    keyDeposits_ = fold ((dsDeposits . dpsDState . lsDPState) ls)
    poolDeposits_ = fold ((psDeposits . dpsPState . lsDPState) ls)

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
        -- keyDepositAdaPot,  -- We don't count these two, as their
        -- poolDepositAdaPot, -- sum is always depositsAdaPot
        depositsAdaPot,
        feesAdaPot
      } = totalAdaPotsES cs

-- =============================================
-- Produced and Consumed are specialized AdaPots
-- relative to the actions of a TxBody

-- | Itemizing what is consumed by a transaction
data Consumed = Consumed
  {conInputs :: !Coin, conRefunds :: !Coin, conWithdrawals :: !Coin}

instance Show Consumed where
  show (Consumed (Coin i) (Coin r) (Coin w)) =
    "Consumed(Inputs " ++ show i ++ ", Refunds " ++ show r ++ ", Withdrawals " ++ show w ++ ") = " ++ show (i + r + w)

-- | Itemizing what is Produced by a transaction
data Produced = Produced
  {proOutputs :: !Coin, proFees :: !Coin, proDeposits :: !Coin}

instance Show Produced where
  show (Produced (Coin out) (Coin f) (Coin d)) =
    "Produced(Outputs " ++ show out ++ ", Fees " ++ show f ++ " Deposits " ++ show d ++ ") = " ++ show (out + f + d)

-- =========================

-- | Compute the Coin part of what is consumed by a TxBody, itemized as a 'Consume'
consumedTxBody ::
  ( HasField "_keyDeposit" pp Coin,
    ShelleyEraTxBody era
  ) =>
  TxBody era ->
  pp ->
  DPState (EraCrypto era) ->
  UTxO era ->
  Consumed
consumedTxBody txBody pp dpstate (UTxO u) = Consumed {conInputs = i, conRefunds = r, conWithdrawals = w}
  where
    i = coinBalance (UTxO (Map.restrictKeys u (txBody ^. inputsTxBodyL)))
    r = keyTxRefunds pp dpstate txBody
    w = fold . unWdrl $ txBody ^. wdrlsTxBodyL

-- | Compute the Coin part of what is produced by a TxBody, itemized as a 'Produced'
producedTxBody ::
  ( ShelleyEraTxBody era,
    HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin
  ) =>
  TxBody era ->
  pp ->
  DPState (EraCrypto era) ->
  Produced
producedTxBody txBody pp dpstate = Produced {proOutputs = out, proFees = f, proDeposits = d}
  where
    out = coinBalance (txouts txBody)
    f = txBody ^. feeTxBodyL
    d = totalTxDeposits pp dpstate txBody
