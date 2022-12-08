{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.LedgerState.DPState
  ( DPState (..),
    DState (..),
    PState (..),
    InstantaneousRewards (..),
    FutureGenDeleg (..),
    rewards,
    delegations,
    ptrsMap,
  )
where

import Cardano.Ledger.Binary
  ( FromCBOR (..),
    FromSharedCBOR (..),
    Interns,
    ToCBOR (..),
    decodeRecordNamed,
    decodeRecordNamedT,
    encodeListLen,
    fromNotSharedCBOR,
    fromSharedPlusCBOR,
    fromSharedPlusLensCBOR,
    toMemptyLens,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    DeltaCoin (..),
  )
import Cardano.Ledger.Credential (Credential (..), Ptr)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
  ( GenDelegPair (..),
    GenDelegs (..),
    KeyHash (..),
    KeyRole (..),
  )
import Cardano.Ledger.Shelley.TxBody
  ( PoolParams (..),
  )
import Cardano.Ledger.Slot
  ( EpochNo (..),
    SlotNo (..),
  )
import Cardano.Ledger.UnifiedMap (UMap (UnifiedMap), UnifiedMap, View (Delegations, Rewards), ViewMap)
import Control.DeepSeq (NFData)
import Control.Monad.Trans
import Data.Default.Class (Default (def))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.UMap as UM
import GHC.Generics (Generic)
import Lens.Micro (_1, _2)
import NoThunks.Class (NoThunks (..))

data FutureGenDeleg c = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo,
    fGenDelegGenKeyHash :: !(KeyHash 'Genesis c)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureGenDeleg c)

instance NFData (FutureGenDeleg c)

instance CC.Crypto c => ToCBOR (FutureGenDeleg c) where
  toCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> toCBOR a <> toCBOR b

instance CC.Crypto c => FromCBOR (FutureGenDeleg c) where
  fromCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> fromCBOR <*> fromCBOR

-- | InstantaneousRewards captures the pending changes to the ledger
-- state caused by MIR certificates. It consists of two mappings,
-- the rewards which will be paid out from the reserves and the rewards
-- which will be paid out from the treasury. It also consists of
-- two coin values which represent the transfer of coins from
-- one pot to the other pot.
-- NOTE that the following property should always hold:
--   deltaReserves + deltaTreasury = 0
data InstantaneousRewards c = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking c) Coin),
    iRTreasury :: !(Map (Credential 'Staking c) Coin),
    deltaReserves :: !DeltaCoin,
    deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c)

instance NFData (InstantaneousRewards c)

-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState c = DState
  { -- | Unified Reward Maps. This contains the reward map (which is the source
    -- of truth regarding the registered stake credentials, the delegation map,
    -- and the stake credential pointer map.
    dsUnified :: !(UnifiedMap c),
    -- | Future genesis key delegations
    dsFutureGenDelegs :: !(Map (FutureGenDeleg c) (GenDelegPair c)),
    -- | Genesis key delegations
    dsGenDelegs :: !(GenDelegs c),
    -- | Instantaneous Rewards
    dsIRewards :: !(InstantaneousRewards c)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DState c)

instance NFData (InstantaneousRewards c) => NFData (DState c)

instance (CC.Crypto c, ToCBOR (InstantaneousRewards c)) => ToCBOR (DState c) where
  toCBOR (DState unified fgs gs ir) =
    encodeListLen 4
      <> toCBOR unified
      <> toCBOR fgs
      <> toCBOR gs
      <> toCBOR ir

instance (CC.Crypto c, FromSharedCBOR (InstantaneousRewards c)) => FromSharedCBOR (DState c) where
  type
    Share (DState c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  fromSharedPlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      unified <- fromSharedPlusCBOR
      fgs <- lift fromCBOR
      gs <- lift fromCBOR
      ir <- fromSharedPlusLensCBOR _1
      pure $ DState unified fgs gs ir

-- | The state used by the POOL rule, which tracks stake pool information.
data PState c = PState
  { -- | The stake pool parameters.
    psStakePoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c)),
    -- | The future stake pool parameters.
    -- Changes to existing stake pool parameters are staged in order
    -- to give delegators time to react to changes.
    -- See section 11.2, "Example Illustration of the Reward Cycle",
    -- of the Shelley Ledger Specification for a sequence diagram.
    psFutureStakePoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c)),
    -- | A map of retiring stake pools to the epoch when they retire.
    psRetiring :: !(Map (KeyHash 'StakePool c) EpochNo)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState c)

instance NFData (PState c)

instance CC.Crypto c => ToCBOR (PState c) where
  toCBOR (PState a b c) =
    encodeListLen 3 <> toCBOR a <> toCBOR b <> toCBOR c

instance CC.Crypto c => FromSharedCBOR (PState c) where
  type
    Share (PState c) =
      Interns (KeyHash 'StakePool c)
  fromSharedPlusCBOR = decodeRecordNamedT "PState" (const 3) $ do
    psStakePoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolParams, psFutureStakePoolParams, psRetiring}

instance (CC.Crypto c, FromSharedCBOR (PState c)) => FromCBOR (PState c) where
  fromCBOR = fromNotSharedCBOR

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data DPState c = DPState
  { dpsDState :: !(DState c),
    dpsPState :: !(PState c)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DPState c)

instance NFData (InstantaneousRewards c) => NFData (DPState c)

instance CC.Crypto c => ToCBOR (InstantaneousRewards c) where
  toCBOR (InstantaneousRewards irR irT dR dT) =
    encodeListLen 4 <> toCBOR irR <> toCBOR irT <> toCBOR dR <> toCBOR dT

instance CC.Crypto c => FromSharedCBOR (InstantaneousRewards c) where
  type Share (InstantaneousRewards c) = Interns (Credential 'Staking c)
  fromSharedPlusCBOR =
    decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
      irR <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      irT <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
      dR <- lift fromCBOR
      dT <- lift fromCBOR
      pure $ InstantaneousRewards irR irT dR dT

instance
  CC.Crypto c =>
  ToCBOR (DPState c)
  where
  toCBOR DPState {dpsPState, dpsDState} =
    encodeListLen 2
      <> toCBOR dpsPState -- We get better sharing when encoding pstate before dstate
      <> toCBOR dpsDState

instance CC.Crypto c => FromSharedCBOR (DPState c) where
  type
    Share (DPState c) =
      ( Interns (Credential 'Staking c),
        Interns (KeyHash 'StakePool c)
      )
  fromSharedPlusCBOR = decodeRecordNamedT "DPState" (const 2) $ do
    dpsPState <- fromSharedPlusLensCBOR _2
    dpsDState <- fromSharedPlusCBOR
    pure DPState {dpsPState, dpsDState}

instance Default (DPState c) where
  def = DPState def def

instance Default (InstantaneousRewards c) where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState c) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState c) where
  def =
    PState Map.empty Map.empty Map.empty

rewards :: DState c -> ViewMap c (Credential 'Staking c) Coin
rewards (DState unified _ _ _) = Rewards unified

delegations ::
  DState c ->
  ViewMap c (Credential 'Staking c) (KeyHash 'StakePool c)
delegations (DState unified _ _ _) = Delegations unified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState c -> Map Ptr (Credential 'Staking c)
ptrsMap (DState (UnifiedMap _ ptrmap) _ _ _) = ptrmap
