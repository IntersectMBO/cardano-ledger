{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Rewards (
  RewardType (..),
  Reward (..),
)
where

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeWord,
  encodeWord,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
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
-- <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf formal specification>
-- for more details.
data RewardType = MemberReward | LeaderReward
  deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance NoThunks RewardType

instance NFData RewardType

instance ToJSON RewardType

instance EncCBOR RewardType where
  encCBOR MemberReward = encodeWord 0
  encCBOR LeaderReward = encodeWord 1

instance DecCBOR RewardType where
  decCBOR =
    decodeWord >>= \case
      0 -> pure MemberReward
      1 -> pure LeaderReward
      n -> invalidKey n

-- | The 'Reward' type captures:
--
-- * if the reward is a member or leader reward
--
-- * the stake pool ID associated with the reward
--
-- * the number of Lovelace in the reward
data Reward c = Reward
  { rewardType :: !RewardType
  , rewardPool :: !(KeyHash 'StakePool c)
  , rewardAmount :: !Coin
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

instance Crypto c => EncCBOR (Reward c) where
  encCBOR (Reward rt pool c) =
    encode $ Rec Reward !> To rt !> To pool !> To c

instance Crypto c => DecCBOR (Reward c) where
  decCBOR =
    decode $ RecD Reward <! From <! From <! From

instance Crypto c => ToJSON (Reward c) where
  toJSON = object . toRewardPair
  toEncoding = pairs . mconcat . toRewardPair

toRewardPair :: (KeyValue e a, Crypto c) => Reward c -> [a]
toRewardPair r@(Reward _ _ _) =
  let Reward {..} = r
   in [ "rewardType" .= rewardType
      , "rewardPool" .= rewardPool
      , "rewardAmount" .= rewardAmount
      ]
