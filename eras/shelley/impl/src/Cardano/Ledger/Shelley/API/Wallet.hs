{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- FIXME: use better names for record names
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cardano.Ledger.Shelley.API.Wallet (
  -- * Reward types
  RewardInfoPool (..),
  RewardParams (..),

  -- * Transaction helpers
  addKeyWitnesses,

  -- * Ada pots
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
) where

import Cardano.Ledger.BaseTypes (
  NonNegativeInterval,
  UnitInterval,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeDouble,
  encodeDouble,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Keys (WitVKey (..))
import Cardano.Ledger.Shelley.AdaPots (
  AdaPots (..),
  totalAdaES,
  totalAdaPotsES,
 )
import Cardano.Ledger.Shelley.Core
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- | Information about a stake pool
data RewardInfoPool = RewardInfoPool
  { stake :: Coin
  -- ^ Absolute stake delegated to this pool
  , ownerPledge :: Coin
  -- ^ Pledge of pool owner(s)
  , ownerStake :: Coin
  -- ^ Absolute stake delegated by pool owner(s)
  , cost :: Coin
  -- ^ Pool cost
  , margin :: UnitInterval
  -- ^ Pool margin
  , performanceEstimate :: Double
  -- ^ Number of blocks produced divided by expected number of blocks.
  -- Can be larger than @1.0@ for pool that gets lucky.
  -- (If some pools get unlucky, some pools must get lucky.)
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RewardInfoPool

instance NFData RewardInfoPool

deriving instance FromJSON RewardInfoPool

deriving instance ToJSON RewardInfoPool

-- | Global information that influences stake pool rewards
data RewardParams = RewardParams
  { nOpt :: Word16
  -- ^ Desired number of stake pools
  , a0 :: NonNegativeInterval
  -- ^ Influence of the pool owner's pledge on rewards
  , rPot :: Coin
  -- ^ Total rewards available for the given epoch
  , totalStake :: Coin
  -- ^ Maximum lovelace supply minus treasury
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks RewardParams

instance NFData RewardParams

deriving instance FromJSON RewardParams

deriving instance ToJSON RewardParams

addKeyWitnesses :: EraTx era => Tx t era -> Set (WitVKey Witness) -> Tx t era
addKeyWitnesses tx newWits = tx & witsTxL . addrTxWitsL %~ Set.union newWits

--------------------------------------------------------------------------------
-- CBOR instances
--------------------------------------------------------------------------------

instance EncCBOR RewardParams where
  encCBOR (RewardParams p1 p2 p3 p4) =
    encode $
      Rec RewardParams
        !> To p1
        !> To p2
        !> To p3
        !> To p4

instance DecCBOR RewardParams where
  decCBOR =
    decode $
      RecD RewardParams
        <! From
        <! From
        <! From
        <! From

instance EncCBOR RewardInfoPool where
  encCBOR (RewardInfoPool p1 p2 p3 p4 p5 d6) =
    encode $
      Rec RewardInfoPool
        !> To p1
        !> To p2
        !> To p3
        !> To p4
        !> To p5
        !> E encodeDouble d6

instance DecCBOR RewardInfoPool where
  decCBOR =
    decode $
      RecD RewardInfoPool
        <! From
        <! From
        <! From
        <! From
        <! From
        <! D decodeDouble
