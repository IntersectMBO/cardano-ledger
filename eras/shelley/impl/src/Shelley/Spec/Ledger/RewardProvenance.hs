{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.RewardProvenance
  ( RewardProvenance (..),
    RewardProvenancePool (..),
    Desirability (..),
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeDouble,
    encodeDouble,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.SafeHash (SafeHash, unsafeMakeSafeHash)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coders
  ( Decode (..),
    Encode (..),
    decode,
    encode,
    (!>),
    (<!),
  )
import Data.Default.Class (Default (..))
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..))
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.TxBody (PoolParams (..), RewardAcnt (..))

-- instances only
-- ==========================================================

-- | Provenance for an individual stake pool's reward calculation.
data RewardProvenancePool crypto = RewardProvenancePool
  { -- | The number of blocks the pool produced.
    poolBlocksP :: !Natural,
    -- | The stake pool's stake share (portion of the total stake).
    sigmaP :: !Rational,
    -- | The stake pool's active stake share (portion of the active stake).
    sigmaAP :: !Rational,
    -- | The number of Lovelace owned by the stake pool owners.
    -- If this value is not at least as large as the 'pledgeRatioP',
    -- the stake pool will not earn any rewards for the given epoch.
    ownerStakeP :: !Coin,
    -- | The stake pool's registered parameters.
    poolParamsP :: !(PoolParams crypto),
    -- | The stake pool's pledge.
    pledgeRatioP :: !Rational,
    -- | The maximum number of Lovelace this stake pool can earn.
    maxPP :: !Coin,
    -- | The stake pool's apparent performance.
    -- See Section 5.5.2 of the
    --  <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec Design Specification>.
    appPerfP :: !Rational,
    -- | The total Lovelace earned by the stake pool.
    poolRP :: !Coin,
    -- | The total Lovelace earned by the stake pool leader.
    lRewardP :: !Coin
  }
  deriving (Eq, Generic)

instance NoThunks (RewardProvenancePool crypto)

instance NFData (RewardProvenancePool crypto)

deriving instance (CC.Crypto crypto) => FromJSON (RewardProvenancePool crypto)

deriving instance (CC.Crypto crypto) => ToJSON (RewardProvenancePool crypto)

instance CC.Crypto crypto => Default (RewardProvenancePool crypto) where
  def = RewardProvenancePool 0 0 0 (Coin 0) def 0 (Coin 0) 0 (Coin 0) (Coin 0)

-- | The desirability score of a stake pool, as described
-- in <https://arxiv.org/abs/1807.11218 "Reward Sharing Schemes for Stake Pools">.
-- Additionally, the hit rate estimation described in the
-- <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.pool-ranking/latest/download-by-type/doc-pdf/pool-ranking stake pool ranking document> is included.
data Desirability = Desirability
  { desirabilityScore :: !Double,
    hitRateEstimate :: !Double
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
--  <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec the formal specification>.
--  The variable names here align with those in the specification.
--  See also Section 5 of the
--  <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec Design Specification>.
data RewardProvenance crypto = RewardProvenance
  { -- | The number of slots per epoch.
    spe :: !Word64,
    -- | A map from pool ID (the key hash of the stake pool operator's
    -- verification key) to the number of blocks made in the given epoch.
    blocks :: !(BlocksMade crypto),
    -- | The maximum Lovelace supply. On mainnet, this value is equal to
    -- 45 * 10^15 (45 billion ADA).
    maxLL :: !Coin,
    -- | The maximum amount of Lovelace which can be removed from the reserves
    -- to be given out as rewards for the given epoch.
    deltaR1 :: !Coin,
    -- | The difference between the total Lovelace that could have been
    -- distributed as rewards this epoch (which is 'r') and what was actually distributed.
    deltaR2 :: !Coin,
    -- | The total Lovelace available for rewards for the given epoch,
    -- equal to 'rPot' less 'deltaT1'.
    r :: !Coin,
    -- | The maximum Lovelace supply ('maxLL') less the current value of the reserves.
    totalStake :: !Coin,
    -- | The total number of blocks produced during the given epoch.
    blocksCount :: !Integer,
    -- | The decentralization parameter.
    d :: !Rational,
    -- | The number of blocks expected to be produced during the given epoch.
    expBlocks :: !Integer,
    -- | The ratio of the number of blocks actually made versus the number
    -- of blocks that were expected.
    eta :: !Rational,
    -- | The reward pot for the given epoch, equal to 'deltaR1' plus the fee pot.
    rPot :: !Coin,
    -- | The amount of Lovelace taken from the treasury for the given epoch.
    deltaT1 :: !Coin,
    -- | The amount of Lovelace that is delegated during the given epoch.
    activeStake :: !Coin,
    -- | Individual stake pool provenance.
    pools ::
      !( Map
           (KeyHash 'StakePool crypto)
           (RewardProvenancePool crypto)
       ),
    -- | A map from pool ID to the desirability score.
    -- See the <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.pool-ranking/latest/download-by-type/doc-pdf/pool-ranking stake pool ranking document>.
    desirabilities ::
      !(Map (KeyHash 'StakePool crypto) Desirability)
  }
  deriving (Eq, Generic)

deriving instance FromJSON Desirability

deriving instance ToJSON Desirability

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

instance CC.Crypto crypto => Default (PoolParams crypto) where
  def = PoolParams def def (Coin 0) (Coin 0) def def def def def

instance CC.Crypto e => Default (Credential r e) where
  def = KeyHashObj def

instance CC.Crypto crypto => Default (RewardAcnt crypto) where
  def = RewardAcnt def def

instance CC.Crypto c => Default (SafeHash c i) where
  def = unsafeMakeSafeHash def

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
          "desirabilities = " ++ show (desirabilities t)
        ]

-- =======================================================
-- CBOR instances

instance ToCBOR Desirability where
  toCBOR (Desirability p1 p2) =
    encode $ Rec Desirability !> E encodeDouble p1 !> E encodeDouble p2

instance FromCBOR Desirability where
  fromCBOR = decode $ RecD Desirability <! D decodeDouble <! D decodeDouble

instance
  (CC.Crypto crypto) =>
  ToCBOR (RewardProvenancePool crypto)
  where
  toCBOR (RewardProvenancePool p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
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

instance
  (CC.Crypto crypto) =>
  FromCBOR (RewardProvenancePool crypto)
  where
  fromCBOR =
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

instance
  (CC.Crypto crypto) =>
  ToCBOR (RewardProvenance crypto)
  where
  toCBOR (RewardProvenance p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16) =
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

instance
  (CC.Crypto crypto) =>
  FromCBOR (RewardProvenance crypto)
  where
  fromCBOR =
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
