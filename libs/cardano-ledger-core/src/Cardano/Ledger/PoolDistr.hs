{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The stake distribution, aggregated by stake pool (as opposed to stake credential),
-- plays a primary role in Cardano's proof of stake network.
-- Together with the VRF checks, the stake distribution determines leader election.
-- The leader election is the precisely the part of the ledger that is
-- determined by Ouroboros (Praos and Genesis), our consensus mechanism.
-- See Section 16, "Leader Value Calculation", of the
-- <https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf formal specification>.
module Cardano.Ledger.PoolDistr (
  IndividualPoolStake (..),
  PoolDistr (..),
  poolDistrDistrL,
  poolDistrTotalL,
  individualPoolStakeCoinL,
)
where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF)
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- | The 'IndividualPoolStake' contains all the stake controlled
-- by a single stake pool (the combination of owners and delegates)
-- for a given epoch, and also the hash of the stake pool's
-- registered VRF key.
--
-- When a stake pool produces a block, the header contains the
-- full VRF verification key and VRF value for leader election.
-- We check the VRF key against the value in 'IndividualPoolStake'
-- and we check the VRF value using the epoch nonce and
-- the relative stake of the pool as given in 'IndividualPoolStake'.
-- The stake is relative to the total amount of active stake
-- in the network. Stake is active if it is both registered and
-- delegated to a registered stake pool.
data IndividualPoolStake c = IndividualPoolStake
  { individualPoolStake :: !Rational
  , individualPoolStakeCoin :: !(CompactForm Coin) -- TODO: Document
  , individualPoolStakeVrf :: !(Hash c (VerKeyVRF c))
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

individualPoolStakeCoinL :: Lens' (IndividualPoolStake c) (CompactForm Coin)
individualPoolStakeCoinL = lens individualPoolStakeCoin $ \x y -> x {individualPoolStakeCoin = y}

instance Crypto c => EncCBOR (IndividualPoolStake c) where
  encCBOR (IndividualPoolStake stake stakeCoin vrf) =
    mconcat
      [ encodeListLen 3 -- TODO: This needs versioning!
      , encCBOR stake
      , encCBOR stakeCoin
      , encCBOR vrf
      ]

instance Crypto c => DecCBOR (IndividualPoolStake c) where
  decCBOR =
    decodeRecordNamed "IndividualPoolStake" (const 2) $
      IndividualPoolStake
        <$> decCBOR
        <*> decCBOR
        <*> decCBOR

instance Crypto c => ToJSON (IndividualPoolStake c) where
  toJSON = object . toIndividualPoolStakePair
  toEncoding = pairs . mconcat . toIndividualPoolStakePair

toIndividualPoolStakePair :: (KeyValue e a, Crypto c) => IndividualPoolStake c -> [a]
toIndividualPoolStakePair indivPoolStake@(IndividualPoolStake _ _ _) =
  let IndividualPoolStake {..} = indivPoolStake
   in [ "individualPoolStake" .= individualPoolStake
      , "individualPoolStakeCoin" .= individualPoolStakeCoin
      , "individualPoolStakeVrf" .= individualPoolStakeVrf
      ]

-- | A map of stake pool IDs (the hash of the stake pool operator's
-- verification key) to 'IndividualPoolStake'.
data PoolDistr c = PoolDistr
  { unPoolDistr :: Map (KeyHash 'StakePool c) (IndividualPoolStake c)
  , pdTotal :: !(CompactForm Coin)
  }
  deriving stock (Show, Eq, Generic)
  deriving (NFData, NoThunks, ToJSON)

poolDistrDistrL :: Lens' (PoolDistr c) (Map (KeyHash 'StakePool c) (IndividualPoolStake c))
poolDistrDistrL = lens unPoolDistr $ \x y -> x {unPoolDistr = y}

poolDistrTotalL :: Lens' (PoolDistr c) (CompactForm Coin)
poolDistrTotalL = lens pdTotal $ \x y -> x {pdTotal = y}

instance Crypto c => EncCBOR (PoolDistr c) where
  encCBOR (PoolDistr distr total) =
    -- TODO: @aniketd: This needs versioning!
    encode $
      Rec PoolDistr
        !> To distr
        !> To total

instance Crypto c => DecCBOR (PoolDistr c) where
  decCBOR =
    decode $
      RecD PoolDistr
        <! From
        <! From

-- -- ===============================

-- instance
--   HasExp
--     (PoolDistr c)
--     ( Map
--         (KeyHash 'StakePool c)
--         (IndividualPoolStake c)
--     )
--   where
--   toExp (PoolDistr x) = Base MapR x

-- -- | We can Embed a Newtype around a Map (or other Iterable type) and then use it in a set expression.
-- instance
--   Embed
--     (PoolDistr c)
--     ( Map
--         (KeyHash 'StakePool c)
--         (IndividualPoolStake c)
--     )
--   where
--   toBase (PoolDistr x) = x
--   fromBase = PoolDistr
