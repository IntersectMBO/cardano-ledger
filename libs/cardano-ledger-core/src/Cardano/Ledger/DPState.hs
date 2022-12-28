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
  payKeyDeposit,
  payPoolDeposit,
  refundKeyDeposit,
  refundPoolDeposit,
  obligationDPState,
)
where

import Cardano.Ledger.Binary (
  FromCBOR (..),
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
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
 )
import Cardano.Ledger.Core (EraCrypto, EraPParams, PParams, ppKeyDepositL, ppPoolDepositL)
import Cardano.Ledger.Credential (Credential (..), Ptr)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
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
import Cardano.Ledger.UMapCompact (CompactForm, UMap (UMap), View (Delegations, Rewards))
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
  -- of truth regarding the registered stake credentials, the delegation map,
  -- and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map (FutureGenDeleg c) (GenDelegPair c))
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !(GenDelegs c)
  -- ^ Genesis key delegations
  , dsIRewards :: !(InstantaneousRewards c)
  -- ^ Instantaneous Rewards
  , dsDeposits :: !(Map (Credential 'Staking c) Coin)
  -- ^ The Deposit map for staking credentials
  }
  deriving (Show, Eq, Generic)

instance NoThunks (InstantaneousRewards c) => NoThunks (DState c)

instance NFData (InstantaneousRewards c) => NFData (DState c)

instance (CC.Crypto c, ToCBOR (InstantaneousRewards c)) => ToCBOR (DState c) where
  toCBOR (DState unified fgs gs ir ds) =
    encodeListLen 5
      <> toCBOR unified
      <> toCBOR fgs
      <> toCBOR gs
      <> toCBOR ir
      <> toCBOR ds

instance (CC.Crypto c, FromSharedCBOR (InstantaneousRewards c)) => FromSharedCBOR (DState c) where
  type
    Share (DState c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  fromSharedPlusCBOR =
    decodeRecordNamedT "DState" (const 5) $ do
      unified <- fromSharedPlusCBOR
      fgs <- lift fromCBOR
      gs <- lift fromCBOR
      ir <- fromSharedPlusLensCBOR _1
      ds <- fromSharedPlusLensCBOR (_1 . toMemptyLens _1 id)
      pure $ DState unified fgs gs ir ds

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

instance CC.Crypto c => ToCBOR (PState c) where
  toCBOR (PState a b c d) =
    encodeListLen 4 <> toCBOR a <> toCBOR b <> toCBOR c <> toCBOR d

instance CC.Crypto c => FromSharedCBOR (PState c) where
  type
    Share (PState c) =
      Interns (KeyHash 'StakePool c)
  fromSharedPlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psStakePoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    psDeposits <- fromSharedPlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits}

instance (CC.Crypto c, FromSharedCBOR (PState c)) => FromCBOR (PState c) where
  fromCBOR = fromNotSharedCBOR

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data DPState c = DPState
  { dpsDState :: !(DState c)
  , dpsPState :: !(PState c)
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
      ( Interns (Credential 'Staking c)
      , Interns (KeyHash 'StakePool c)
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
      Map.empty

instance Default (PState c) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

rewards :: DState c -> View c (Credential 'Staking c) (CompactForm Coin)
rewards = Rewards . dsUnified

delegations ::
  DState c ->
  View c (Credential 'Staking c) (KeyHash 'StakePool c)
delegations = Delegations . dsUnified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState c -> Map Ptr (Credential 'Staking c)
ptrsMap (DState {dsUnified = UMap _ ptrmap}) = ptrmap

-- ==========================================================
-- Functions that handle Deposits for stake credentials and key hashes.

-- | One only pays a deposit on the initial key registration. If the key has been
--   de-registered it should have been removed from the map. If it hasn't been
--   de-registered, then it has no effect on the Deposits. Paying a deposit on
--   a credential already registered should have no effect. (In fact it probably
--   should be an error) So to avoid this problem one should make an explicit
--   check that the credential is not registered in the places where this function is called.
payKeyDeposit ::
  EraPParams era =>
  Credential 'Staking (EraCrypto era) ->
  PParams era ->
  DState (EraCrypto era) ->
  DState (EraCrypto era)
payKeyDeposit cred pp dstate = dstate {dsDeposits = newStake}
  where
    stake = dsDeposits dstate
    newStake
      | Map.notMember cred stake = Map.insert cred (pp ^. ppKeyDepositL) stake
      | otherwise = stake

refundKeyDepositFull :: Credential 'Staking c -> DState c -> (Coin, DState c)
refundKeyDepositFull cred dstate =
  (coin, dstate {dsDeposits = newStake})
  where
    stake = dsDeposits dstate
    (coin, newStake) = case Map.lookup cred stake of
      Just c -> (c, Map.delete cred stake)
      Nothing -> (mempty, stake)

refundKeyDeposit :: Credential 'Staking c -> DState c -> DState c
refundKeyDeposit cred dstate = snd (refundKeyDepositFull cred dstate)

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
obligationDPState (DPState DState {dsDeposits = keys} PState {psDeposits = stakePools}) =
  foldl' (<>) (Coin 0) keys <> foldl' (<>) (Coin 0) stakePools

-- =====================================================

instance ToExpr (DPState c)

instance ToExpr (PState c)

instance ToExpr (DState c)

instance ToExpr (FutureGenDeleg c)

instance ToExpr (InstantaneousRewards c)
