{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.DPState (
  DPState (..),
  DState (..),
  PState (..),
  InstantaneousRewards (..),
  FutureGenDeleg (..),
  rewards,
  delegations,
  ptrsMap,
  payPoolDeposit,
  refundPoolDeposit,
  obligationDPState,
)
where

import Cardano.Ledger.Binary.Plain (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decNoShareCBOR,
  decSharePlusCBOR,
  decSharePlusLensCBOR,
  decodeRecordNamed,
  decodeRecordNamedT,
  encodeListLen,
  toMemptyLens,
 )
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
 )
import Cardano.Ledger.Core (EraCrypto, EraPParams, PParams, ppPoolDepositL)
import Cardano.Ledger.Credential (Credential (..), Ptr)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  KeyRole (..),
 )
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Slot (
  EpochNo (..),
  SlotNo (..),
 )
import Cardano.Ledger.TreeDiff (ToExpr)
import Cardano.Ledger.UMapCompact (RDPair (..), UMap (UMap), View (Delegations, RewardDeposits))
import qualified Cardano.Ledger.UMapCompact as UM
import Control.DeepSeq (NFData)
import Control.Monad.Trans
import Data.Default.Class (Default (def))
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Lens.Micro ((^.), _1, _2)
import NoThunks.Class (NoThunks (..))

-- ======================================

data FutureGenDeleg c = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo
  , fGenDelegGenKeyHash :: !(KeyHash 'Genesis c)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (FutureGenDeleg c)

instance NFData (FutureGenDeleg c)

instance Crypto c => EncCBOR (FutureGenDeleg c) where
  encCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> encCBOR a <> encCBOR b

instance Crypto c => DecCBOR (FutureGenDeleg c) where
  decCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> decCBOR <*> decCBOR

-- | InstantaneousRewards captures the pending changes to the ledger
-- state caused by MIR certificates. It consists of two mappings,
-- the rewards which will be paid out from the reserves and the rewards
-- which will be paid out from the treasury. It also consists of
-- two coin values which represent the transfer of coins from
-- one pot to the other pot.
-- NOTE that the following property should always hold:
--   deltaReserves + deltaTreasury = 0
data InstantaneousRewards c = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking c) Coin)
  , iRTreasury :: !(Map (Credential 'Staking c) Coin)
  , deltaReserves :: !DeltaCoin
  , deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c)

instance NFData (InstantaneousRewards c)

-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState c = DState
  { dsUnified :: !(UMap c)
  -- ^ Unified Reward Maps. This contains the reward map (which is the source
  -- of truth regarding the registered stake credentials, the deposit map,
  -- the delegation map, and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map (FutureGenDeleg c) (GenDelegPair c))
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !(GenDelegs c)
  -- ^ Genesis key delegations
  , dsIRewards :: !(InstantaneousRewards c)
  -- ^ Instantaneous Rewards
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DState c)

instance NFData (InstantaneousRewards c) => NFData (DState c)

instance (Crypto c, EncCBOR (InstantaneousRewards c)) => EncCBOR (DState c) where
  encCBOR (DState unified fgs gs ir) =
    encodeListLen 4
      <> encCBOR unified
      <> encCBOR fgs
      <> encCBOR gs
      <> encCBOR ir

instance (Crypto c, DecShareCBOR (InstantaneousRewards c)) => DecShareCBOR (DState c) where
  type
    Share (DState c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  decSharePlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      unified <- decSharePlusCBOR
      fgs <- lift decCBOR
      gs <- lift decCBOR
      ir <- decSharePlusLensCBOR _1
      pure $ DState unified fgs gs ir

-- | The state used by the POOL rule, which tracks stake pool information.
data PState c = PState
  { psStakePoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c))
  -- ^ The stake pool parameters.
  , psFutureStakePoolParams :: !(Map (KeyHash 'StakePool c) (PoolParams c))
  -- ^ The future stake pool parameters.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool c) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool c) Coin)
  -- ^ A map of the deposits for each pool
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState c)

instance NFData (PState c)

instance Crypto c => EncCBOR (PState c) where
  encCBOR (PState a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance Crypto c => DecShareCBOR (PState c) where
  type Share (PState c) = Interns (KeyHash 'StakePool c)
  decSharePlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psStakePoolParams <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolParams <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psDeposits <- decSharePlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits}

instance (Crypto c, DecShareCBOR (PState c)) => DecCBOR (PState c) where
  decCBOR = decNoShareCBOR

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data DPState c = DPState
  { dpsDState :: !(DState c)
  , dpsPState :: !(PState c)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DPState c)

instance NFData (InstantaneousRewards c) => NFData (DPState c)

instance Crypto c => EncCBOR (InstantaneousRewards c) where
  encCBOR (InstantaneousRewards irR irT dR dT) =
    encodeListLen 4 <> encCBOR irR <> encCBOR irT <> encCBOR dR <> encCBOR dT

instance Crypto c => DecShareCBOR (InstantaneousRewards c) where
  type Share (InstantaneousRewards c) = Interns (Credential 'Staking c)
  decSharePlusCBOR =
    decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
      irR <- decSharePlusLensCBOR (toMemptyLens _1 id)
      irT <- decSharePlusLensCBOR (toMemptyLens _1 id)
      dR <- lift decCBOR
      dT <- lift decCBOR
      pure $ InstantaneousRewards irR irT dR dT

instance Crypto c => EncCBOR (DPState c) where
  encCBOR DPState {dpsPState, dpsDState} =
    encodeListLen 2
      <> encCBOR dpsPState -- We get better sharing when encoding pstate before dstate
      <> encCBOR dpsDState

instance Crypto c => DecShareCBOR (DPState c) where
  type
    Share (DPState c) =
      ( Interns (Credential 'Staking c)
      , Interns (KeyHash 'StakePool c)
      )
  decSharePlusCBOR = decodeRecordNamedT "DPState" (const 2) $ do
    dpsPState <- decSharePlusLensCBOR _2
    dpsDState <- decSharePlusCBOR
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
    PState Map.empty Map.empty Map.empty Map.empty

rewards :: DState c -> View c (Credential 'Staking c) RDPair
rewards = RewardDeposits . dsUnified

delegations ::
  DState c ->
  View c (Credential 'Staking c) (KeyHash 'StakePool c)
delegations = Delegations . dsUnified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState c -> Map Ptr (Credential 'Staking c)
ptrsMap (DState {dsUnified = UMap _ ptrmap}) = ptrmap

-- ==========================================================
-- Functions that handle Deposits for pool key hashes.

-- | One only pays a deposit on the initial pool registration. So return the
--   the Deposits unchanged if the keyhash already exists. There are legal
--   situations where a pool may be registered multiple times.
payPoolDeposit ::
  EraPParams era =>
  KeyHash 'StakePool (EraCrypto era) ->
  PParams era ->
  PState (EraCrypto era) ->
  PState (EraCrypto era)
payPoolDeposit keyhash pp pstate = pstate {psDeposits = newpool}
  where
    pool = psDeposits pstate
    newpool
      | Map.notMember keyhash pool = Map.insert keyhash (pp ^. ppPoolDepositL) pool
      | otherwise = pool

refundPoolDeposit :: KeyHash 'StakePool c -> PState c -> (Coin, PState c)
refundPoolDeposit keyhash pstate = (coin, pstate {psDeposits = newpool})
  where
    pool = psDeposits pstate
    (coin, newpool) = case Map.lookup keyhash pool of
      Just c -> (c, Map.delete keyhash pool)
      Nothing -> (mempty, pool)

-- | Calculate total possible refunds in the system. There is an invariant that
--   this should be the same as the utxosDeposited field of the UTxOState. Note that
--   this does not depend upon the current values of the Key and Pool deposits of the PParams.
obligationDPState :: DPState era -> Coin
obligationDPState (DPState DState {dsUnified = umap} PState {psDeposits = stakePools}) =
  UM.fromCompact (UM.sumDepositView (RewardDeposits umap)) <> foldl' (<>) (Coin 0) stakePools

-- =====================================================

instance ToExpr (DPState c)

instance ToExpr (PState c)

instance ToExpr (DState c)

instance ToExpr (FutureGenDeleg c)

instance ToExpr (InstantaneousRewards c)
