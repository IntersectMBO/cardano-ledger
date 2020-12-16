{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.RewardProvenance where

import qualified Cardano.Ledger.Crypto as CC
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (..))
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.TxBody (PoolParams (..), RewardAcnt (..))

-- instances only
-- ==========================================================

data RewardProvenancePool crypto = RewardProvenancePool
  { poolBlocksP :: !Natural,
    sigmaP :: !Rational,
    sigmaAP :: !Rational,
    ownerStakeP :: !Coin,
    poolParamsP :: !(PoolParams crypto),
    pledgeRatioP :: !Rational,
    maxPP :: !Coin,
    appPerfP :: !Rational,
    poolRP :: !Coin,
    --mRewardsP :: !(Map (Credential 'Staking crypto) Coin),
    lRewardP :: !Coin
  }
  deriving (Eq, Generic)

instance NoThunks (RewardProvenancePool crypto)

instance NFData (RewardProvenancePool crypto)

deriving instance (CC.Crypto crypto) => FromJSON (RewardProvenancePool crypto)

deriving instance (CC.Crypto crypto) => ToJSON (RewardProvenancePool crypto)

instance Default (RewardProvenancePool crypto) where
  def = RewardProvenancePool 0 0 0 (Coin 0) def 0 (Coin 0) 0 (Coin 0) (Coin 0)

data RewardProvenance crypto = RewardProvenance
  { spe :: !Word64,
    blocks :: !(BlocksMade crypto),
    maxLL :: !Coin,
    deltaR1 :: !Coin,
    deltaR2 :: !Coin,
    r :: !Coin,
    totalStake :: !Coin,
    blocksCount :: !Integer,
    d :: !Rational,
    expBlocks :: !Integer,
    eta :: !Rational,
    rPot :: !Coin,
    deltaT1 :: !Coin,
    activeStake :: !Coin,
    pools ::
      !( Map
           (KeyHash 'StakePool crypto)
           (RewardProvenancePool crypto)
       ),
    hitRateEstimates :: !(Map (KeyHash 'StakePool crypto) (Double, Double))
    -- The pair of Doubles represents (LiklihoodEstimate,Desireability)
  }
  deriving (Eq, Generic)

deriving instance (CC.Crypto crypto) => FromJSON (RewardProvenance crypto)

deriving instance (CC.Crypto crypto) => ToJSON (RewardProvenance crypto)

instance NoThunks (RewardProvenance crypto)

instance NFData (RewardProvenance crypto)

instance Default (RewardProvenance crypto) where
  def =
    RewardProvenance
      0
      (BlocksMade def)
      (Coin 0)
      (Coin 0)
      (Coin 0)
      (Coin 0)
      (Coin 0)
      0
      0
      0
      0
      (Coin 0)
      (Coin 0)
      (Coin 0)
      def
      def

instance Default (PoolParams crypto) where
  def = PoolParams def def (Coin 0) (Coin 0) def def def def def

instance Default (Credential r e) where
  def = KeyHashObj def

instance Default (RewardAcnt crypto) where
  def = RewardAcnt def def

-- =======================================================
-- Show instances

mylines :: Int -> [String] -> String
mylines n xs = unlines (map (replicate n ' ' ++) xs)

instance Show (RewardProvenancePool crypto) where
  show t =
    "RewardProvenancePool\n"
      ++ mylines
        3
        [ "poolBlocks = " ++ show (poolBlocksP t),
          "sigma = " ++ show (sigmaP t),
          "sigmaA = " ++ show (sigmaAP t),
          "ownerStake = " ++ show (ownerStakeP t),
          "poolParams = " ++ showPoolParams (poolParamsP t),
          "pledgeRatio = " ++ show (pledgeRatioP t),
          "maxP = " ++ show (maxPP t),
          "appPerf = " ++ show (appPerfP t),
          "poolR = " ++ show (poolRP t),
          -- "mRewards = "++ show (mRewardsP t)
          "lReward = " ++ show (lRewardP t)
        ]

showPoolParams :: PoolParams crypto -> String
showPoolParams x =
  "PoolParams\n"
    ++ mylines
      6
      [ "poolId = " ++ show (_poolId x),
        "poolVrf = " ++ show (_poolVrf x),
        "poolPledge = " ++ show (_poolPledge x),
        "poolCost = " ++ show (_poolCost x),
        "poolMargin = " ++ show (_poolMargin x),
        "poolRAcnt = " ++ show (_poolRAcnt x),
        "poolOwners = " ++ show (_poolOwners x),
        "poolRelays = " ++ show (_poolRelays x),
        "poolMD = " ++ show (_poolMD x)
      ]

instance Show (RewardProvenance crypto) where
  show t =
    "RewardProvenance\n"
      ++ mylines
        3
        [ "spe = " ++ show (spe t),
          "blocks = " ++ show (blocks t),
          "maxLL = " ++ show (maxLL t),
          "deltaR1 = " ++ show (deltaR1 t),
          "deltaR2 = " ++ show (deltaR2 t),
          "r = " ++ show (r t),
          "totalStake = " ++ show (totalStake t),
          "blocksCount = " ++ show (blocksCount t),
          "d = " ++ show (d t),
          "expBlocks = " ++ show (expBlocks t),
          "eta = " ++ show (eta t),
          "rPot = " ++ show (rPot t),
          "deltaT1 = " ++ show (deltaT1 t),
          "activeStake = " ++ show (activeStake t),
          "pools = " ++ show (pools t),
          "hitRateEstimates = " ++ show (hitRateEstimates t)
        ]
