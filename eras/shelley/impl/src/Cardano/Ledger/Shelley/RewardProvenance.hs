{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.RewardProvenance (
  RewardProvenance (..),
  RewardProvenancePool (..),
  Desirability (..),
)
where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- instances only
-- ==========================================================

-- | Provenance for an individual stake pool's reward calculation.
data RewardProvenancePool c = RewardProvenancePool
  { poolBlocksP :: !Natural
  -- ^ The number of blocks the pool produced.
  , sigmaP :: !Rational
  -- ^ The stake pool's stake share (portion of the total stake).
  , sigmaAP :: !Rational
  -- ^ The stake pool's active stake share (portion of the active stake).
  , ownerStakeP :: !Coin
  -- ^ The number of Lovelace owned by the stake pool owners.
  -- If this value is not at least as large as the 'pledgeRatioP',
  -- the stake pool will not earn any rewards for the given epoch.
  , poolParamsP :: !(PoolParams c)
  -- ^ The stake pool's registered parameters.
  , pledgeRatioP :: !Rational
  -- ^ The stake pool's pledge.
  , maxPP :: !Coin
  -- ^ The maximum number of Lovelace this stake pool can earn.
  , appPerfP :: !Rational
  -- ^ The stake pool's apparent performance.
  -- See Section 5.5.2 of the
  --  <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf>
  , poolRP :: !Coin
  -- ^ The total Lovelace earned by the stake pool.
  , lRewardP :: !Coin
  -- ^ The total Lovelace earned by the stake pool leader.
  }
  deriving (Eq, Generic)

instance NoThunks (RewardProvenancePool c)

instance NFData (RewardProvenancePool c)

deriving instance Crypto c => FromJSON (RewardProvenancePool c)

deriving instance Crypto c => ToJSON (RewardProvenancePool c)

instance Crypto c => Default (RewardProvenancePool c) where
  def = RewardProvenancePool 0 0 0 (Coin 0) def 0 (Coin 0) 0 (Coin 0) (Coin 0)

-- | The desirability score of a stake pool, as described
-- in <https://arxiv.org/abs/1807.11218 "Reward Sharing Schemes for Stake Pools">.
-- Additionally, the hit rate estimation described in the
-- <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf stake pool ranking document> is included.
data Desirability = Desirability
  { desirabilityScore :: !Double
  , hitRateEstimate :: !Double
  }
  deriving (Eq, Show, Generic)

instance NoThunks Desirability

instance NFData Desirability

-- | 'RewardProvenenace' captures some of the intermediate calculations when computing
--     the staking reward distribution. Most of these fields are simple scalar
--     values, computed from the current State, and are fixed before we start to compute
--     the distribution. Two of them are aggregates computed when we compute the distribution
--     ('pools' and 'desirabilities').
--
--  For more background, see "Figure 48: The Reward Calculation" and
--  "Figure 51: Reward Update Creation" of the
--  <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf the formal specification>.
--  The variable names here align with those in the specification.
--  See also Section 5 of the
--  <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-delegation.pdf>
data RewardProvenance c = RewardProvenance
  { spe :: !Word64
  -- ^ The number of slots per epoch.
  , blocks :: !(BlocksMade c)
  -- ^ A map from pool ID (the key hash of the stake pool operator's
  -- verification key) to the number of blocks made in the given epoch.
  , maxLL :: !Coin
  -- ^ The maximum Lovelace supply. On mainnet, this value is equal to
  -- 45 * 10^15 (45 billion ADA).
  , deltaR1 :: !Coin
  -- ^ The maximum amount of Lovelace which can be removed from the reserves
  -- to be given out as rewards for the given epoch.
  , deltaR2 :: !Coin
  -- ^ The difference between the total Lovelace that could have been
  -- distributed as rewards this epoch (which is 'r') and what was actually distributed.
  , r :: !Coin
  -- ^ The total Lovelace available for rewards for the given epoch,
  -- equal to 'rPot' less 'deltaT1'.
  , totalStake :: !Coin
  -- ^ The maximum Lovelace supply ('maxLL') less the current value of the reserves.
  , blocksCount :: !Integer
  -- ^ The total number of blocks produced during the given epoch.
  , d :: !Rational
  -- ^ The decentralization parameter.
  , expBlocks :: !Integer
  -- ^ The number of blocks expected to be produced during the given epoch.
  , eta :: !Rational
  -- ^ The ratio of the number of blocks actually made versus the number
  -- of blocks that were expected.
  , rPot :: !Coin
  -- ^ The reward pot for the given epoch, equal to 'deltaR1' plus the fee pot.
  , deltaT1 :: !Coin
  -- ^ The amount of Lovelace taken from the treasury for the given epoch.
  , activeStake :: !Coin
  -- ^ The amount of Lovelace that is delegated during the given epoch.
  , pools ::
      !( Map
          (KeyHash 'StakePool c)
          (RewardProvenancePool c)
       )
  -- ^ Individual stake pool provenance.
  , desirabilities ::
      !(Map (KeyHash 'StakePool c) Desirability)
  -- ^ A map from pool ID to the desirability score.
  -- See the <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/pool-ranking.pdf stake pool ranking document>.
  }
  deriving (Eq, Generic)

deriving instance FromJSON Desirability

deriving instance ToJSON Desirability

deriving instance Crypto c => FromJSON (RewardProvenance c)

deriving instance Crypto c => ToJSON (RewardProvenance c)

instance NoThunks (RewardProvenance c)

instance NFData (RewardProvenance c)

instance Default (RewardProvenance c) where
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

-- =======================================================
-- Show instances

mylines :: Int -> [String] -> String
mylines n xs = unlines (map (replicate n ' ' ++) xs)

instance Show (RewardProvenancePool c) where
  show t =
    "RewardProvenancePool\n"
      ++ mylines
        3
        [ "poolBlocks = " ++ show (poolBlocksP t)
        , "sigma = " ++ show (sigmaP t)
        , "sigmaA = " ++ show (sigmaAP t)
        , "ownerStake = " ++ show (ownerStakeP t)
        , "poolParams = " ++ showPoolParams (poolParamsP t)
        , "pledgeRatio = " ++ show (pledgeRatioP t)
        , "maxP = " ++ show (maxPP t)
        , "appPerf = " ++ show (appPerfP t)
        , "poolR = " ++ show (poolRP t)
        , "lReward = " ++ show (lRewardP t)
        ]

showPoolParams :: PoolParams c -> String
showPoolParams x =
  "PoolParams\n"
    ++ mylines
      6
      [ "poolId = " ++ show (ppId x)
      , "poolVrf = " ++ show (ppVrf x)
      , "poolPledge = " ++ show (ppPledge x)
      , "poolCost = " ++ show (ppCost x)
      , "poolMargin = " ++ show (ppMargin x)
      , "poolRAcnt = " ++ show (ppRewardAccount x)
      , "poolOwners = " ++ show (ppOwners x)
      , "poolRelays = " ++ show (ppRelays x)
      , "poolMD = " ++ show (ppMetadata x)
      ]

instance Show (RewardProvenance c) where
  show t =
    "RewardProvenance\n"
      ++ mylines
        3
        [ "spe = " ++ show (spe t)
        , "blocks = " ++ show (blocks t)
        , "maxLL = " ++ show (maxLL t)
        , "deltaR1 = " ++ show (deltaR1 t)
        , "deltaR2 = " ++ show (deltaR2 t)
        , "r = " ++ show (r t)
        , "totalStake = " ++ show (totalStake t)
        , "blocksCount = " ++ show (blocksCount t)
        , "d = " ++ show (d t)
        , "expBlocks = " ++ show (expBlocks t)
        , "eta = " ++ show (eta t)
        , "rPot = " ++ show (rPot t)
        , "deltaT1 = " ++ show (deltaT1 t)
        , "activeStake = " ++ show (activeStake t)
        , "pools = " ++ show (pools t)
        , "desirabilities = " ++ show (desirabilities t)
        ]

-- =======================================================
-- CBOR instances

instance EncCBOR Desirability where
  encCBOR (Desirability p1 p2) =
    encode $ Rec Desirability !> To p1 !> To p2

instance DecCBOR Desirability where
  decCBOR = decode $ RecD Desirability <! From <! From

instance Crypto c => EncCBOR (RewardProvenancePool c) where
  encCBOR (RewardProvenancePool p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
    encode $
      Rec RewardProvenancePool
        !> To p1
        !> To p2
        !> To p3
        !> To p4
        !> To p5
        !> To p6
        !> To p7
        !> To p8
        !> To p9
        !> To p10

instance Crypto c => DecCBOR (RewardProvenancePool c) where
  decCBOR =
    decode $
      RecD RewardProvenancePool
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From

instance Crypto c => EncCBOR (RewardProvenance c) where
  encCBOR (RewardProvenance p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16) =
    encode $
      Rec RewardProvenance
        !> To p1
        !> To p2
        !> To p3
        !> To p4
        !> To p5
        !> To p6
        !> To p7
        !> To p8
        !> To p9
        !> To p10
        !> To p11
        !> To p12
        !> To p13
        !> To p14
        !> To p15
        !> To p16

instance Crypto c => DecCBOR (RewardProvenance c) where
  decCBOR =
    decode $
      RecD RewardProvenance
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
        <! From
