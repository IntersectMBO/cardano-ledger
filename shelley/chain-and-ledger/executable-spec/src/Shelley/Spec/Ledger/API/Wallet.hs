{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.API.Wallet
  ( getNonMyopicMemberRewards,
    getUTxO,
    getFilteredUTxO,
    getLeaderSchedule,
    getTotalStake,
    poolsByTotalStakeFraction,
  )
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto (VRF)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyBased)
import Cardano.Slotting.EpochInfo (epochInfoRange)
import Cardano.Slotting.Slot (SlotNo)
import qualified Data.ByteString.Short as BSS
import Data.Foldable (fold)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API.Protocol (ChainDepState (..))
import Shelley.Spec.Ledger.API.Validation (ShelleyState)
import Shelley.Spec.Ledger.Address (Addr (..), serialiseAddr)
import Shelley.Spec.Ledger.BaseTypes (Globals (..), Seed)
import Shelley.Spec.Ledger.BlockChain (checkLeaderValue, mkSeed, seedL)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..), PoolDistr (..))
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..), SignKeyVRF)
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
    circulation,
    stakeDistr,
  )
import Shelley.Spec.Ledger.OverlaySchedule (isOverlaySlot)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.Rewards
  ( NonMyopic (..),
    StakeShare (..),
    getTopRankedPools,
    nonMyopicMemberRew,
    percentile',
  )
import Shelley.Spec.Ledger.STS.NewEpoch (calculatePoolDistr)
import Shelley.Spec.Ledger.STS.Tickn (TicknState (..))
import Shelley.Spec.Ledger.TxBody (PoolParams (..), TxOut (..))
import Shelley.Spec.Ledger.UTxO (UTxO (..))

-- | Get pool sizes, but in terms of total stake
--
-- The stake distribution uses active stake (so that the leader schedule is not
-- affected by undelegated stake), but the wallet wants to display pool
-- saturation for rewards purposes. For that, it needs the fraction of total
-- stake.
--
-- This is not based on any snapshot, but uses the current ledger state.
poolsByTotalStakeFraction ::
  forall era.
  ShelleyBased era =>
  Globals ->
  ShelleyState era ->
  PoolDistr (Crypto era)
poolsByTotalStakeFraction globals ss =
  PoolDistr poolsByTotalStake
  where
    snap@(EB.SnapShot stake _ _) = currentSnapshot ss
    Coin totalStake = getTotalStake globals ss
    Coin activeStake = fold . EB.unStake $ stake
    stakeRatio = activeStake % totalStake
    PoolDistr poolsByActiveStake = calculatePoolDistr snap
    poolsByTotalStake = Map.map toTotalStakeFrac poolsByActiveStake
    toTotalStakeFrac :: IndividualPoolStake (Crypto era) -> IndividualPoolStake (Crypto era)
    toTotalStakeFrac (IndividualPoolStake s vrf) =
      IndividualPoolStake (s * stakeRatio) vrf

-- | Calculate the current total stake.
getTotalStake :: Globals -> ShelleyState era -> Coin
getTotalStake globals ss =
  let supply = Coin . fromIntegral $ maxLovelaceSupply globals
      es = nesEs ss
   in circulation es supply

-- | Calculate the Non-Myopic Pool Member Rewards for a set of credentials.
-- For each given credential, this function returns a map from each stake
-- pool (identified by the key hash of the pool operator) to the
-- non-myopic pool member reward for that stake pool.
--
-- This is not based on any snapshot, but uses the current ledger state.
getNonMyopicMemberRewards ::
  ShelleyBased era =>
  Globals ->
  ShelleyState era ->
  Set (Either Coin (Credential 'Staking era)) ->
  Map (Either Coin (Credential 'Staking era)) (Map (KeyHash 'StakePool (Crypto era)) Coin)
getNonMyopicMemberRewards globals ss creds =
  Map.fromList $
    fmap
      (\cred -> (cred, Map.map (mkNMMRewards $ memShare cred) poolData))
      (Set.toList creds)
  where
    maxSupply = Coin . fromIntegral $ maxLovelaceSupply globals
    Coin totalStake = circulation es maxSupply
    toShare (Coin x) = StakeShare (x % totalStake)
    memShare (Right cred) = toShare $ Map.findWithDefault (Coin 0) cred (EB.unStake stake)
    memShare (Left coin) = toShare coin
    es = nesEs ss
    pp = esPp es
    NonMyopic
      { likelihoodsNM = ls,
        rewardPotNM = rPot
      } = esNonMyopic es
    EB.SnapShot stake delegs poolParams = currentSnapshot ss
    poolData =
      Map.mapWithKey
        ( \k p ->
            ( percentile' (histLookup k),
              p,
              toShare . fold
                . EB.unStake
                $ EB.poolStake k delegs stake
            )
        )
        poolParams
    histLookup k = fromMaybe mempty (Map.lookup k ls)
    topPools = getTopRankedPools rPot (Coin totalStake) pp poolParams (fmap percentile' ls)
    mkNMMRewards t (hitRateEst, poolp, sigma) =
      if checkPledge poolp
        then nonMyopicMemberRew pp rPot poolp s sigma t topPools hitRateEst
        else mempty
      where
        s = (toShare . _poolPledge) poolp
        checkPledge pool =
          let ostake =
                Set.foldl'
                  ( \c o ->
                      c
                        <> ( fromMaybe mempty $
                               Map.lookup (KeyHashObj o) (EB.unStake stake)
                           )
                  )
                  mempty
                  (_poolOwners pool)
           in _poolPledge poolp <= ostake

-- | Create a current snapshot of the ledger state.
--
-- When ranking pools, and reporting their saturation level, in the wallet, we
-- do not want to use one of the regular snapshots, but rather the most recent
-- ledger state.
currentSnapshot :: ShelleyBased era => ShelleyState era -> EB.SnapShot era
currentSnapshot ss =
  stakeDistr utxo dstate pstate
  where
    es = nesEs ss
    utxo = _utxo . _utxoState . esLState $ es
    dstate = _dstate . _delegationState . esLState $ es
    pstate = _pstate . _delegationState . esLState $ es

-- | Get the full UTxO.
getUTxO ::
  ShelleyState era ->
  UTxO era
getUTxO = _utxo . _utxoState . esLState . nesEs

-- | Get the UTxO filtered by address.
getFilteredUTxO ::
  ShelleyState era ->
  Set (Addr era) ->
  UTxO era
getFilteredUTxO ss addrs =
  UTxO $ Map.filter (\(TxOutCompact addrSBS _) -> addrSBS `Set.member` addrSBSs) fullUTxO
  where
    UTxO fullUTxO = getUTxO ss
    -- Instead of decompacting each address in the huge UTxO, compact each
    -- address in the small set of address.
    addrSBSs = Set.map (BSS.toShort . serialiseAddr) addrs

-- | Get the (private) leader schedule for this epoch.
--
--   Given a private VRF key, returns the set of slots in which this node is
--   eligible to lead.
getLeaderSchedule ::
  ( Era era,
    VRF.Signable
      (VRF (Crypto era))
      Seed
  ) =>
  Globals ->
  ShelleyState era ->
  ChainDepState (Crypto era) ->
  KeyHash 'StakePool (Crypto era) ->
  SignKeyVRF (Crypto era) ->
  PParams era ->
  Set SlotNo
getLeaderSchedule globals ss cds poolHash key pp = Set.filter isLeader epochSlots
  where
    isLeader slotNo =
      let y = VRF.evalCertified () (mkSeed seedL slotNo epochNonce) key
       in not (isOverlaySlot a (_d pp) slotNo)
            && checkLeaderValue (VRF.certifiedOutput y) stake f
    stake = maybe 0 individualPoolStake $ Map.lookup poolHash poolDistr
    poolDistr = unPoolDistr $ nesPd ss
    TicknState epochNonce _ = csTickn cds
    currentEpoch = nesEL ss
    ei = epochInfo globals
    f = activeSlotCoeff globals
    epochSlots = Set.fromList [a .. b]
    (a, b) = runIdentity $ epochInfoRange ei currentEpoch
