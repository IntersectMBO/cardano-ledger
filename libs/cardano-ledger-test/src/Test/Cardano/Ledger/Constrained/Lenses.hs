{-# LANGUAGE DataKinds #-}

-- | Define Lenses that facilitate accessing the types in the Var Model.
--   Note the types in the Model, are often wrapped in a newtype in the real state,
--   or they are embedded in something like UMap to save space. So we need interesting
--   Lenses to make this possible.
--   Many other (more standard) Lenses are defined in Cardano.Ledger.Shelley.LedgerState
module Test.Cardano.Ledger.Constrained.Lenses where

import Cardano.Ledger.BaseTypes (SlotNo)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Compactible (Compactible (fromCompact))
import Cardano.Ledger.Core (DRep)
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState hiding (deltaReserves, deltaTreasury, rewards)
import qualified Cardano.Ledger.Shelley.LedgerState as LS (deltaReserves, deltaTreasury)
import Cardano.Ledger.Shelley.PoolRank (Likelihood (..), LogWeight (..), NonMyopic (..))
import Cardano.Ledger.UMap (
  RDPair (..),
  UMap (..),
  compactCoinOrError,
  dRepMap,
  depositMap,
  invPtrMap,
  rewardMap,
  sPoolMap,
  unify,
 )
import qualified Cardano.Ledger.UMap as UM
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (fromList)
import Data.Set (Set)
import Lens.Micro

-- ====================================================
-- Lenses
{- A lens for a record field name 'mm'  looks like this.
   Of course the types will change. This example makes a lens
   for an Int field inside of NewEpoch state

mmL :: Lens' (NewEpochState era) Int
mmL = lens mm (\ds u -> ds { mm = u })

-}

-- ===================================
-- InstantaneousRewards

iRReservesL :: Lens' (InstantaneousRewards c) (Map (Credential 'Staking c) Coin)
iRReservesL = lens iRReserves (\ds u -> ds {iRReserves = u})

iRTreasuryL :: Lens' (InstantaneousRewards c) (Map (Credential 'Staking c) Coin)
iRTreasuryL = lens iRTreasury (\ds u -> ds {iRTreasury = u})

deltaReservesL :: Lens' (InstantaneousRewards c) DeltaCoin
deltaReservesL = lens LS.deltaReserves (\ds u -> ds {LS.deltaReserves = u})

deltaTreasuryL :: Lens' (InstantaneousRewards c) DeltaCoin
deltaTreasuryL = lens LS.deltaTreasury (\ds u -> ds {LS.deltaTreasury = u})

unGenDelegsL :: Lens' (GenDelegs c) (Map (KeyHash 'Genesis c) (GenDelegPair c))
unGenDelegsL = lens unGenDelegs (\(GenDelegs _) new -> GenDelegs new)

-- Lenses for (FutureGenDeleg c)
fGenDelegSlotL :: Lens' (FutureGenDeleg c) SlotNo
fGenDelegSlotL = lens fGenDelegSlot (\ds u -> ds {fGenDelegSlot = u})

fGenDelegGenKeyHashL :: Lens' (FutureGenDeleg c) (KeyHash 'Genesis c)
fGenDelegGenKeyHashL = lens fGenDelegGenKeyHash (\ds u -> ds {fGenDelegGenKeyHash = u})

-- IncrementalStake

isCredMapL :: Lens' (IncrementalStake c) (Map (Credential 'Staking c) Coin)
isCredMapL = lens (fmap fromCompact . credMap) (\ds u -> ds {credMap = fmap compactCoinOrError u})

isPtrMapL :: Lens' (IncrementalStake c) (Map Ptr Coin)
isPtrMapL = lens (fmap fromCompact . ptrMap) (\ds u -> ds {ptrMap = fmap compactCoinOrError u})

-- ===============================================
-- NonMyopic

nmLikelihoodsL :: Lens' (NonMyopic c) (Map (KeyHash 'StakePool c) [Float])
nmLikelihoodsL =
  lens
    (fmap fromLikelihood . likelihoodsNM)
    (\ds u -> ds {likelihoodsNM = fmap toLikelihood u})
  where
    fromLikelihood (Likelihood ls) = fmap unLogWeight $ toList ls
    toLikelihood = Likelihood . fromList . fmap LogWeight

nmRewardPotL :: Lens' (NonMyopic c) Coin
nmRewardPotL = lens rewardPotNM (\ds u -> ds {rewardPotNM = u})

-- ======================================================
-- (Virtual) UMap

spRewL :: Lens' (Split c) (Map (Credential 'Staking c) Coin)
spRewL = lens spRew (\ds u -> ds {spRew = u})

spDepL :: Lens' (Split c) (Map (Credential 'Staking c) Coin)
spDepL = lens spDep (\ds u -> ds {spDep = u})

spDelL :: Lens' (Split c) (Map (Credential 'Staking c) (KeyHash 'StakePool c))
spDelL = lens spDel (\ds u -> ds {spDel = u})

spRevPtrL :: Lens' (Split c) (Map (Credential 'Staking c) (Set Ptr))
spRevPtrL = lens spRevPtr (\ds u -> ds {spRevPtr = u})

spPtrL :: Lens' (Split c) (Map Ptr (Credential 'Staking c))
spPtrL = lens spPtr (\ds u -> ds {spPtr = u})

-- ========================================================================================
-- Mapping the abstract names: rewards, delegations, ptrs, credDeposits through the UMap

-- | Abstract view of the UMap
data Split c = Split
  { spRew :: Map (Credential 'Staking c) Coin
  , spDep :: Map (Credential 'Staking c) Coin
  , spDel :: Map (Credential 'Staking c) (KeyHash 'StakePool c)
  , spDRep :: Map (Credential 'Staking c) (DRep c)
  , spRevPtr :: Map (Credential 'Staking c) (Set Ptr)
  , spPtr :: Map Ptr (Credential 'Staking c)
  }

-- | The abstraction function, from concrete (UMap) to abstract (Split)
splitUMap :: UMap c -> Split c
splitUMap um =
  Split (rewardMap um) (depositMap um) (sPoolMap um) (dRepMap um) (invPtrMap um) (UM.ptrMap um)

-- | The concretization function from abstract (Split) to concrete (UMap)
unSplitUMap :: Split c -> UMap c
unSplitUMap (Split rew dep deleg drep _revptr ptr) = unify (merge rew dep) ptr deleg drep
  where
    merge x y | Map.keysSet x /= Map.keysSet y = error "different domains"
    merge x y = Map.intersectionWith rdpair x y
    rdpair x y = RDPair (compactCoinOrError x) (compactCoinOrError y)

-- Lenses that reach through the concrete  (UMap) using abstract inputs

rewardsUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) Coin)
rewardsUMapL = lens rewardMap delta
  where
    delta um new = unSplitUMap (split {spRew = new})
      where
        split = splitUMap um

stakeDepositsUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) Coin)
stakeDepositsUMapL = lens depositMap delta
  where
    delta um new = unSplitUMap (split {spDep = new})
      where
        split = splitUMap um

ptrsUMapL :: Lens' (UMap c) (Map Ptr (Credential 'Staking c))
ptrsUMapL = lens UM.ptrMap (\(UMap x _) p -> UMap x p)

delegationsUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) (KeyHash 'StakePool c))
delegationsUMapL = lens sPoolMap delta
  where
    delta um new = unSplitUMap (split {spDel = new})
      where
        split = splitUMap um
