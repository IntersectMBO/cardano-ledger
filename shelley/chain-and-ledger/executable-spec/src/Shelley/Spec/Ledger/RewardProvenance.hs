{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shelley.Spec.Ledger.RewardProvenance where

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
    lRewardP :: !Coin
  }
  deriving (Eq, Generic)

instance NoThunks (RewardProvenancePool crypto)

instance NFData (RewardProvenancePool crypto)

deriving instance (CC.Crypto crypto) => FromJSON (RewardProvenancePool crypto)

deriving instance (CC.Crypto crypto) => ToJSON (RewardProvenancePool crypto)

instance Default (RewardProvenancePool crypto) where
  def = RewardProvenancePool 0 0 0 (Coin 0) def 0 (Coin 0) 0 (Coin 0) (Coin 0)

data Desirability = Desirability
  { desirabilityScore :: !Double,
    hitRateEstimate :: !Double
  }
  deriving (Eq, Show, Generic)

instance NoThunks Desirability

instance NFData Desirability

-- | RewardProvenenace captures some of the intermediate calculations when computimg
--     the statking reward distribution, most of these fields are simple scalar
--     values, computed from the current State, and are fixed before we start to compute
--     the distribution. 3 of them are aggregates computed when we compute the distribution.
data RewardProvenance crypto = RewardProvenance
  { spe :: !Word64,
    blocks :: !(BlocksMade crypto),
    maxLL :: !Coin,
    deltaR1 :: !Coin,
    deltaR2 :: !Coin, -- Aggregate
    r :: !Coin,
    totalStake :: !Coin,
    blocksCount :: !Integer,
    d :: !Rational,
    expBlocks :: !Integer,
    eta :: !Rational,
    rPot :: !Coin,
    deltaT1 :: !Coin,
    activeStake :: !Coin,
    pools :: -- Aggregate
      !( Map
           (KeyHash 'StakePool crypto)
           (RewardProvenancePool crypto)
       ),
    desirabilities :: -- Aggregate
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

instance Default (PoolParams crypto) where
  def = PoolParams def def (Coin 0) (Coin 0) def def def def def

instance Default (Credential r e) where
  def = KeyHashObj def

instance Default (RewardAcnt crypto) where
  def = RewardAcnt def def

instance Default (SafeHash c i) where
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
