{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Rewards (
  RewardType (..),
  Reward (..),
)
where

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Binary.Plain as Plain (
  DecCBOR (..),
  EncCBOR (..),
  decodeWord,
  encodeWord,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | The staking rewards in Cardano are all either:
--
-- * member rewards - rewards given to a registered stake credential which has delegated
-- to a stake pool, or
--
-- * leader rewards - rewards given to a registered stake pool (in particular, given to the
-- stake credential in the stake pool registration certificate).
--
-- See Figure 47, "Functions used in the Reward Splitting", of the
-- <https://github.com/input-output-hk/cardano-ledger/releases/latest/download/shelley-ledger.pdf formal specification>
-- for more details.
data RewardType = MemberReward | LeaderReward
  deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance NoThunks RewardType

instance NFData RewardType

instance EncCBOR RewardType where
  encCBOR MemberReward = Plain.encodeWord 0
  encCBOR LeaderReward = Plain.encodeWord 1

-- instance ToCBOR RewardType

instance DecCBOR RewardType where
  decCBOR =
    Plain.decodeWord >>= \case
      0 -> pure MemberReward
      1 -> pure LeaderReward
      n -> invalidKey n

-- instance FromCBOR RewardType

-- | The 'Reward' type captures:
--
-- * if the reward is a member or leader reward
--
-- * the stake pool ID associated with the reward
--
-- * the number of Lovelace in the reward
data Reward c = Reward
  { rewardType :: RewardType
  , rewardPool :: KeyHash 'StakePool c
  , rewardAmount :: Coin
  }
  deriving (Eq, Show, Generic)

-- | Note that this Ord instance is chosen to align precisely
--  with the Allegra reward aggregation, as given by the
--  function 'aggregateRewards' so that 'Set.findMax' returns
--  the expected value.
instance Ord (Reward c) where
  compare (Reward MemberReward _ _) (Reward LeaderReward _ _) = GT
  compare (Reward LeaderReward _ _) (Reward MemberReward _ _) = LT
  compare (Reward _ pool1 _) (Reward _ pool2 _) = compare pool1 pool2

instance NoThunks (Reward c)

instance NFData (Reward c)

instance Crypto c => ToCBOR (Reward c) where
  toCBOR Reward {rewardType,rewardPool, rewardType, rewardAmount} =
    toCBOR (rewardType,rewardPool, rewardType, rewardAmount)

instance Crypto c => FromCBOR (Reward c) where
  fromCBOR =
    decode $ RecD Reward <! From <! From <! From
