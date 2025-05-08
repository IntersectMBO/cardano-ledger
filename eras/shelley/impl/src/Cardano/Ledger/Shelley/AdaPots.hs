{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
  Produced (..),
  Consumed (..),
  consumedTxBody,
  producedTxBody,
  sumAdaPots,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState.Types (
  EpochState (..),
  LedgerState (..),
  UTxOState (..),
  lsCertStateL,
  lsUTxOStateL,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.TxBody (unWithdrawals)
import Cardano.Ledger.State
import Cardano.Ledger.UMap (sumRewardsUView)
import Control.DeepSeq (NFData)
import Data.Foldable (fold)
import GHC.Generics (Generic)
import Lens.Micro ((^.))

data AdaPots = AdaPots
  { treasuryAdaPot :: Coin
  , reservesAdaPot :: Coin
  , rewardsAdaPot :: Coin
  , utxoAdaPot :: Coin
  , feesAdaPot :: Coin
  , obligationsPot :: Obligations
  }
  deriving (Show, Eq, Generic)

instance NFData AdaPots

-- | Calculate the total ada pots in the epoch state
totalAdaPotsES ::
  ( EraTxOut era
  , EraGov era
  , EraCertState era
  ) =>
  EpochState era ->
  AdaPots
totalAdaPotsES (EpochState (ChainAccountState {casTreasury, casReserves}) ls _ _) =
  AdaPots
    { treasuryAdaPot = casTreasury
    , reservesAdaPot = casReserves
    , rewardsAdaPot = rewards_
    , utxoAdaPot = coins
    , feesAdaPot = fees_
    , obligationsPot = obligationCertState certState <> govStateObligations
    }
  where
    UTxOState u _ fees_ _ _ _ = lsUTxOState ls
    certState = ls ^. lsCertStateL
    rewards_ = fromCompact $ sumRewardsUView (rewards $ certState ^. certDStateL)
    coins = sumCoinUTxO u
    govStateObligations = obligationGovState (ls ^. lsUTxOStateL . utxosGovStateL)

sumAdaPots :: AdaPots -> Coin
sumAdaPots
  AdaPots
    { treasuryAdaPot
    , reservesAdaPot
    , rewardsAdaPot
    , utxoAdaPot
    , feesAdaPot
    , obligationsPot
    } =
    treasuryAdaPot
      <> reservesAdaPot
      <> rewardsAdaPot
      <> utxoAdaPot
      <> feesAdaPot
      <> sumObligation obligationsPot

-- | Calculate the total ada in the epoch state
totalAdaES :: (EraTxOut era, EraGov era, EraCertState era) => EpochState era -> Coin
totalAdaES = sumAdaPots . totalAdaPotsES

-- =============================================
-- Produced and Consumed are specialized AdaPots
-- relative to the actions of a TxBody

-- | Itemizing what is consumed by a transaction
data Consumed = Consumed
  {conInputs :: !Coin, conRefunds :: !Coin, conWithdrawals :: !Coin}

instance Show Consumed where
  show (Consumed (Coin i) (Coin r) (Coin w)) =
    "Consumed(Inputs "
      ++ show i
      ++ ", Refunds "
      ++ show r
      ++ ", Withdrawals "
      ++ show w
      ++ ") = "
      ++ show (i + r + w)

-- | Itemizing what is Produced by a transaction
data Produced = Produced
  { proOutputs :: !Coin
  , proFees :: !Coin
  , proDeposits :: !Coin
  }

instance Show Produced where
  show (Produced (Coin out) (Coin f) (Coin d)) =
    "Produced(Outputs "
      ++ show out
      ++ ", Fees "
      ++ show f
      ++ ", Deposits "
      ++ show d
      ++ ") = "
      ++ show (out + f + d)

-- =========================

-- | Compute the Coin part of what is consumed by a TxBody, itemized as a 'Consume'
consumedTxBody ::
  (EraTxBody era, EraCertState era) =>
  TxBody era ->
  PParams era ->
  CertState era ->
  UTxO era ->
  Consumed
consumedTxBody txBody pp dpstate utxo =
  Consumed
    { conInputs =
        sumCoinUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
    , conRefunds = certsTotalRefundsTxBody pp dpstate txBody
    , conWithdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL
    }

-- | Compute the Coin part of what is produced by a TxBody, itemized as a 'Produced'
producedTxBody ::
  (EraTxBody era, EraCertState era) =>
  TxBody era ->
  PParams era ->
  CertState era ->
  Produced
producedTxBody txBody pp dpstate =
  Produced
    { proOutputs = sumCoinUTxO (txouts txBody)
    , proFees = txBody ^. feeTxBodyL
    , proDeposits = certsTotalDepositsTxBody pp dpstate txBody
    }
