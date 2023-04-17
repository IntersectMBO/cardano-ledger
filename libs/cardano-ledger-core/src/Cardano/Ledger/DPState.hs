{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.DPState (
  DPState (..),
  DState (..),
  lookupDepositDState,
  lookupRewardDState,
  PState (..),
  InstantaneousRewards (..),
  FutureGenDeleg (..),
  rewards,
  delegations,
  ptrsMap,
  payPoolDeposit,
  refundPoolDeposit,
  obligationDPState,
  Diff (..),
)
where

import Cardano.Ledger.Binary (
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
  Diff (DiffCoin),
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (EraCrypto, EraPParams, PParams, ppPoolDepositL)
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeCredential)
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
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (def))
import Data.Foldable (foldl')
import Data.Incremental (ILC (..), MonoidMap (..), insertD, unMM, ($$))
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

instance Crypto c => ToJSON (FutureGenDeleg c) where
  toJSON fGenDeleg =
    object
      [ "fGenDelegSlot" .= fGenDelegSlot fGenDeleg
      , "fGenDelegGenKeyHash" .= fGenDelegGenKeyHash fGenDeleg
      ]

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

instance ILC (InstantaneousRewards c) where
  data Diff (InstantaneousRewards c) = InstantaneousRewards'
    { diffReserves :: !(Diff (MonoidMap (Credential 'Staking c) Coin))
    , diffTreasury :: !(Diff (MonoidMap (Credential 'Staking c) Coin))
    , diffDeltaReserves :: !(Diff Coin)
    , diffDeltaTreasury :: !(Diff Coin)
    }
    deriving (Eq, Show)
  applyDiff (InstantaneousRewards w x (DeltaCoin y) (DeltaCoin z)) (InstantaneousRewards' wd xd (DiffCoin yd) (DiffCoin zd)) =
    InstantaneousRewards
      (unMM (applyDiff (MM w) wd))
      (unMM (applyDiff (MM x) xd))
      (DeltaCoin (y + yd))
      (DeltaCoin (z + zd))
  zero = InstantaneousRewards' zero zero zero zero
  extend (InstantaneousRewards' w x y z) (InstantaneousRewards' a b c d) =
    InstantaneousRewards' (extend w a) (extend x b) (extend y c) (extend z d)
  totalDiff (InstantaneousRewards w x (DeltaCoin y) (DeltaCoin z)) =
    InstantaneousRewards' (totalDiff (MM w)) (totalDiff (MM x)) (totalDiff (Coin y)) (totalDiff (Coin z))

instance NoThunks (InstantaneousRewards c)

instance NFData (InstantaneousRewards c)

instance Crypto c => ToJSON (InstantaneousRewards c) where
  toJSON = object . toInstantaneousRewardsPair
  toEncoding = pairs . mconcat . toInstantaneousRewardsPair

toInstantaneousRewardsPair :: (KeyValue a, Crypto c) => InstantaneousRewards c -> [a]
toInstantaneousRewardsPair InstantaneousRewards {..} =
  [ "iRReserves" .= iRReserves
  , "iRTreasury" .= iRTreasury
  , "deltaReserves" .= deltaReserves
  , "deltaTreasury" .= deltaTreasury
  ]

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

instance ILC (DState c) where
  data Diff (DState c)
    = DState'
        !(Diff (UMap c))
        !(Diff (Map (FutureGenDeleg c) (GenDelegPair c)))
        !(Diff (Map (KeyHash 'Genesis c) (GenDelegPair c)))
        !(Diff (InstantaneousRewards c))
    deriving (Eq, Show)
  applyDiff (DState u f (GenDelegs g) i) (DState' ud fd gd iD) =
    DState (u $$ ud) (f $$ fd) (GenDelegs (g $$ gd)) (i $$ iD)
  zero = DState' zero zero zero zero
  extend (DState' w x y z) (DState' a b c d) = DState' (extend w a) (extend x b) (extend y c) (extend z d)
  totalDiff (DState w x (GenDelegs y) z) =
    DState' (totalDiff w) (totalDiff x) (totalDiff y) (totalDiff z)

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

instance Crypto c => ToJSON (DState c) where
  toJSON = object . toDStatePair
  toEncoding = pairs . mconcat . toDStatePair

toDStatePair :: (KeyValue a, Crypto c) => DState c -> [a]
toDStatePair DState {..} =
  [ "unified" .= dsUnified
  , "fGenDelegs" .= Map.toList dsFutureGenDelegs
  , "genDelegs" .= dsGenDelegs
  , "irwd" .= dsIRewards
  ]

-- | Function that looks up the deposit for currently delegated staking credential
lookupDepositDState :: DState c -> (StakeCredential c -> Maybe Coin)
lookupDepositDState dstate =
  let currentRewardDeposits = RewardDeposits $ dsUnified dstate
   in \k -> do
        RDPair _ deposit <- UM.lookup k currentRewardDeposits
        Just $! fromCompact deposit

-- | Function that looks up curret reward for the delegated staking credential.
lookupRewardDState :: DState c -> (StakeCredential c -> Maybe Coin)
lookupRewardDState dstate =
  let currentRewardDeposits = RewardDeposits $ dsUnified dstate
   in \k -> do
        RDPair reward _ <- UM.lookup k currentRewardDeposits
        Just $! fromCompact reward

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

instance ILC (PState c) where
  data Diff (PState c) = PState'
    { diffPsStakePoolParams :: !(Diff (Map (KeyHash 'StakePool c) (PoolParams c)))
    , diffPsFutureStakePoolParams :: !(Diff (Map (KeyHash 'StakePool c) (PoolParams c)))
    , diffPsRetiring :: !(Diff (Map (KeyHash 'StakePool c) EpochNo))
    , diffPsDeposits :: !(Diff (Map (KeyHash 'StakePool c) Coin))
    }
    deriving (Eq, Show)
  applyDiff (PState w x y z) (PState' wd xd yd zd) =
    PState (w $$ wd) (x $$ xd) (y $$ yd) (z $$ zd)
  zero = PState' zero zero zero zero
  extend (PState' w x y z) (PState' a b c d) =
    PState' (w `extend` a) (x `extend` b) (y `extend` c) (z `extend` d)
  totalDiff (PState w x y z) =
    PState' (totalDiff w) (totalDiff x) (totalDiff y) (totalDiff z)

instance Default (Diff (PState c)) where
  def = undefined

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

instance Crypto c => ToJSON (PState c) where
  toJSON = object . toPStatePair
  toEncoding = pairs . mconcat . toPStatePair

toPStatePair :: (KeyValue a, Crypto c) => PState c -> [a]
toPStatePair PState {..} =
  [ "stakePoolParams" .= psStakePoolParams
  , "futureStakePoolParams" .= psFutureStakePoolParams
  , "retiring" .= psRetiring
  , "deposits" .= psDeposits
  ]

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data DPState c = DPState
  { dpsDState :: !(DState c)
  , dpsPState :: !(PState c)
  }
  deriving (Show, Eq, Generic)

instance ILC (DPState c) where
  data Diff (DPState c) = DPState' (Diff (DState c)) (Diff (PState c))
    deriving (Eq, Show)
  applyDiff (DPState d p) (DPState' dD pD) = DPState (d $$ dD) (p $$ pD)
  zero = DPState' zero zero
  extend (DPState' x y) (DPState' a b) = DPState' (extend x a) (extend y b)
  totalDiff (DPState w x) =
    DPState' (totalDiff w) (totalDiff x)

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
  type Share (DPState c) = (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  decSharePlusCBOR = decodeRecordNamedT "DPState" (const 2) $ do
    dpsPState <- decSharePlusLensCBOR _2
    dpsDState <- decSharePlusCBOR
    pure DPState {dpsPState, dpsDState}

instance Default (DPState c) where
  def = DPState def def

instance Crypto c => ToJSON (DPState c) where
  toJSON = object . toDPStatePairs
  toEncoding = pairs . mconcat . toDPStatePairs

toDPStatePairs :: (KeyValue a, Crypto c) => DPState c -> [a]
toDPStatePairs DPState {..} =
  [ "dstate" .= dpsDState
  , "pstate" .= dpsPState
  ]

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
  Diff (PState (EraCrypto era)) ->
  Diff (PState (EraCrypto era))
payPoolDeposit keyhash pp pstate pstate' = pstate' {diffPsDeposits = newpool <> diffPsDeposits pstate'}
  where
    pool = psDeposits pstate $$ diffPsDeposits pstate'
    newpool
      | Map.notMember keyhash pool = insertD keyhash (pp ^. ppPoolDepositL)
      | otherwise = zero

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
