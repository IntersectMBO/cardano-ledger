{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Define Lenses that facilitate accessing the types in the Var Model.
--   Note the types in the Model, are often wrapped in a newtype in the real state,
--   or they are embedded in something like UMap to save space. So we need interesting
--   Lenses to make this possible.
--   Many other (more standard) Lenses are defined in Cardano.Ledger.Shelley.LedgerState
module Test.Cardano.Ledger.Constrained.Lenses where

import Cardano.Ledger.BaseTypes (SlotNo)
import Cardano.Ledger.Coin (Coin (..), CompactForm, DeltaCoin)
import Cardano.Ledger.Compactible (Compactible (fromCompact))
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.DRep (DRep)
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair (..), GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState hiding (deltaReserves, deltaTreasury, rewards)
import qualified Cardano.Ledger.Shelley.LedgerState as LS (deltaReserves, deltaTreasury)
import Cardano.Ledger.Shelley.PoolRank (Likelihood (..), LogWeight (..), NonMyopic (..))
import Cardano.Ledger.TxIn (TxIn (..))
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
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Classes (TxOutF (..), liftUTxO)
import Test.Cardano.Ledger.Generic.Proof (Proof)

-- ====================================================
-- Lenses
{- A lens for a record field name 'mm'  looks like this.
   Of course the types will change. This example makes a lens
   for an Int field inside of NewEpoch state

mmL :: Lens' (NewEpochState era) Int
mmL = lens mm (\ds u -> ds { mm = u })

-}

updateWithLens :: Lens' a b -> (b -> b) -> a -> a
updateWithLens l f a = a & l .~ f (a ^. l)

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

spDRepL :: Lens' (Split c) (Map (Credential 'Staking c) (DRep c))
spDRepL = lens spDRep (\ds u -> ds {spDRep = u})

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

drepUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) (DRep c))
drepUMapL = lens dRepMap delta
  where
    delta um new = unSplitUMap (split {spDRep = new})
      where
        split = splitUMap um

-- Conversion Lenses

strictMaybeToMaybeL :: Lens' (StrictMaybe x) (Maybe x)
strictMaybeToMaybeL = lens ff gg
  where
    ff (SJust x) = Just x
    ff SNothing = Nothing
    gg _ (Just x) = SJust x
    gg _ Nothing = SNothing

idLens :: Lens' a a
idLens = lens (\x -> x) (\_ y -> y)

strictSeqListL :: Lens' (StrictSeq a) [a]
strictSeqListL = lens toList (\_ y -> fromList y)

mapCompactFormCoinL :: Lens' (Map a (CompactForm Coin)) (Map a Coin)
mapCompactFormCoinL = lens (Map.map fromCompact) (\_ y -> Map.map compactCoinOrError y)

drepDelegationUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) (DRep c))
drepDelegationUMapL = lens dRepMap delta
  where
    delta um new = unSplitUMap (split {spDRep = new})
      where
        split = splitUMap um

pairL :: Lens' x y -> Lens' a b -> Lens' (x, a) (y, b)
pairL xy ab = lens getter setter
  where
    getter (x, a) = (x ^. xy, a ^. ab)
    setter (x, a) (y, b) = (x & xy .~ y, a & ab .~ b)

-- | Lens to convert from the abstract type UtxO type of the Model, to the concrete UTxO type.
--   The mode uses the type family abstraction TxOutF, and does not wrap the map
--   with the UtxO constructor. Note the getter is 'liftUTxO' from Test.Cardano.Ledger.Constrained.Classes
--   liftUTxO :: Map (TxIn (EraCrypto era)) (TxOutF era) -> UTxO era
utxoFL :: Proof era -> Lens' (Map (TxIn (EraCrypto era)) (TxOutF era)) (UTxO era)
utxoFL p = lens liftUTxO (\_ (UTxO new) -> (Map.map (TxOutF p) new))
