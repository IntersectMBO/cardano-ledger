{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.CertState (
  CertState (..),
  DState (..),
  PState (..),
  VState (..),
  InstantaneousRewards (..),
  FutureGenDeleg (..),
  lookupDepositDState,
  lookupRewardDState,
  rewards,
  delegations,
  ptrsMap,
  payPoolDeposit,
  refundPoolDeposit,
  obligationCertState,
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
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (
  Coin (..),
  DeltaCoin (..),
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (Era, EraCrypto, EraPParams, PParams, ppPoolDepositL)
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
import Cardano.Ledger.UMap (RDPair (..), UMap (UMap), UView (RewDepUView, SPoolUView))
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData)
import Control.Monad.Trans
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (def))
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
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
data DState era = DState
  { dsUnified :: !(UMap (EraCrypto era))
  -- ^ Unified Reward Maps. This contains the reward map (which is the source
  -- of truth regarding the registered stake credentials, the deposit map,
  -- the delegation map, and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !(GenDelegs (EraCrypto era))
  -- ^ Genesis key delegations
  , dsIRewards :: !(InstantaneousRewards (EraCrypto era))
  -- ^ Instantaneous Rewards
  }
  deriving (Show, Eq, Generic)

instance NoThunks (DState era)

instance NFData (DState era)

instance (Era era, EncCBOR (InstantaneousRewards (EraCrypto era))) => EncCBOR (DState era) where
  encCBOR (DState unified fgs gs ir) =
    encodeListLen 4
      <> encCBOR unified
      <> encCBOR fgs
      <> encCBOR gs
      <> encCBOR ir

instance (Era era, DecShareCBOR (InstantaneousRewards (EraCrypto era))) => DecShareCBOR (DState era) where
  type
    Share (DState era) =
      (Interns (Credential 'Staking (EraCrypto era)), Interns (KeyHash 'StakePool (EraCrypto era)))
  decSharePlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      unified <- decSharePlusCBOR
      fgs <- lift decCBOR
      gs <- lift decCBOR
      ir <- decSharePlusLensCBOR _1
      pure $ DState unified fgs gs ir

instance Era era => ToJSON (DState era) where
  toJSON = object . toDStatePair
  toEncoding = pairs . mconcat . toDStatePair

toDStatePair :: (KeyValue a, Era era) => DState era -> [a]
toDStatePair DState {..} =
  [ "unified" .= dsUnified
  , "fGenDelegs" .= Map.toList dsFutureGenDelegs
  , "genDelegs" .= dsGenDelegs
  , "irwd" .= dsIRewards
  ]

-- | Function that looks up the deposit for currently delegated staking credential
lookupDepositDState :: DState era -> (StakeCredential (EraCrypto era) -> Maybe Coin)
lookupDepositDState dstate =
  let currentRewardDeposits = RewDepUView $ dsUnified dstate
   in \k -> do
        RDPair _ deposit <- UM.lookup k currentRewardDeposits
        Just $! fromCompact deposit

-- | Function that looks up curret reward for the delegated staking credential.
lookupRewardDState :: DState era -> (StakeCredential (EraCrypto era) -> Maybe Coin)
lookupRewardDState dstate =
  let currentRewardDeposits = RewDepUView $ dsUnified dstate
   in \k -> do
        RDPair reward _ <- UM.lookup k currentRewardDeposits
        Just $! fromCompact reward

-- | The state used by the POOL rule, which tracks stake pool information.
data PState era = PState
  { psStakePoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  -- ^ The stake pool parameters.
  , psFutureStakePoolParams :: !(Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
  -- ^ The future stake pool parameters.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool (EraCrypto era)) Coin)
  -- ^ A map of the deposits for each pool
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState era)

instance NFData (PState era)

instance Era era => EncCBOR (PState era) where
  encCBOR (PState a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance Era era => DecShareCBOR (PState era) where
  type Share (PState era) = Interns (KeyHash 'StakePool (EraCrypto era))
  decSharePlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psStakePoolParams <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolParams <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psDeposits <- decSharePlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits}

instance (Era era, DecShareCBOR (PState era)) => DecCBOR (PState era) where
  decCBOR = decNoShareCBOR

instance Era era => ToJSON (PState era) where
  toJSON = object . toPStatePair
  toEncoding = pairs . mconcat . toPStatePair

toPStatePair :: (KeyValue a, Era era) => PState era -> [a]
toPStatePair PState {..} =
  [ "stakePoolParams" .= psStakePoolParams
  , "futureStakePoolParams" .= psFutureStakePoolParams
  , "retiring" .= psRetiring
  , "deposits" .= psDeposits
  ]

data VState era = VState
  { vsDReps :: !(Set (Credential 'Voting (EraCrypto era)))
  , vsCommitteeHotKeys ::
      !( Map
          (KeyHash 'CommitteeColdKey (EraCrypto era))
          (Maybe (Credential 'CommitteeHotKey (EraCrypto era))) -- `Nothing` to indicate "resigned".
       )
  }
  deriving (Show, Eq, Generic)

instance Default (VState era) where
  def = VState def def

instance NoThunks (VState era)

instance NFData (VState era)

instance Era era => DecCBOR (VState era) where
  decCBOR =
    decode $
      RecD VState
        <! From
        <! From

instance Era era => EncCBOR (VState era) where
  encCBOR VState {..} =
    encode $
      Rec (VState @era)
        !> To vsDReps
        !> To vsCommitteeHotKeys

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
data CertState era = CertState
  { certVState :: !(VState era)
  , certPState :: !(PState era)
  , certDState :: !(DState era)
  }
  deriving (Show, Eq, Generic)

instance NoThunks (CertState c)

instance NFData (CertState c)

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

instance Era era => EncCBOR (CertState era) where
  encCBOR CertState {certPState, certDState, certVState} =
    encodeListLen 3
      <> encCBOR certVState
      <> encCBOR certPState
      <> encCBOR certDState

instance Era era => DecShareCBOR (CertState era) where
  type Share (CertState era) = (Interns (Credential 'Staking (EraCrypto era)), Interns (KeyHash 'StakePool (EraCrypto era)))
  decSharePlusCBOR = decodeRecordNamedT "CertState" (const 3) $ do
    certVState <- lift decCBOR -- TODO: add sharing of DRep credentials
    certPState <- decSharePlusLensCBOR _2
    certDState <- decSharePlusCBOR
    pure CertState {certPState, certDState, certVState}

instance Default (CertState era) where
  def = CertState def def def

instance Era era => ToJSON (CertState era) where
  toJSON = object . toCertStatePairs
  toEncoding = pairs . mconcat . toCertStatePairs

toCertStatePairs :: (KeyValue a, Era era) => CertState era -> [a]
toCertStatePairs CertState {..} =
  [ "dstate" .= certDState
  , "pstate" .= certPState
  ]

instance Default (InstantaneousRewards c) where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState era) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState c) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

rewards :: DState era -> UView (EraCrypto era) (Credential 'Staking (EraCrypto era)) RDPair
rewards = RewDepUView . dsUnified

delegations ::
  DState era ->
  UView (EraCrypto era) (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
delegations = SPoolUView . dsUnified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState era -> Map Ptr (Credential 'Staking (EraCrypto era))
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
  PState era ->
  PState era
payPoolDeposit keyhash pp pstate = pstate {psDeposits = newpool}
  where
    pool = psDeposits pstate
    newpool
      | Map.notMember keyhash pool = Map.insert keyhash (pp ^. ppPoolDepositL) pool
      | otherwise = pool

refundPoolDeposit :: KeyHash 'StakePool (EraCrypto era) -> PState era -> (Coin, PState era)
refundPoolDeposit keyhash pstate = (coin, pstate {psDeposits = newpool})
  where
    pool = psDeposits pstate
    (coin, newpool) = case Map.lookup keyhash pool of
      Just c -> (c, Map.delete keyhash pool)
      Nothing -> (mempty, pool)

-- | Calculate total possible refunds in the system. There is an invariant that
--   this should be the same as the utxosDeposited field of the UTxOState. Note that
--   this does not depend upon the current values of the Key and Pool deposits of the PParams.
obligationCertState :: CertState era -> Coin
obligationCertState (CertState VState {} PState {psDeposits = stakePools} DState {dsUnified = umap}) =
  UM.fromCompact (UM.sumDepositUView (RewDepUView umap)) <> foldl' (<>) (Coin 0) stakePools

-- =====================================================

instance ToExpr (CertState era)

instance ToExpr (PState era)

instance ToExpr (DState era)

instance ToExpr (VState era)

instance ToExpr (FutureGenDeleg c)

instance ToExpr (InstantaneousRewards c)
