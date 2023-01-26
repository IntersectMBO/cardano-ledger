{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.Constrained.Lenses where

-- import Cardano.Ledger.Core (EraTxOut, TxOut, Value)
-- import Cardano.Ledger.DPState (DPState (..), DState (..), FutureGenDeleg, InstantaneousRewards, PState (..))
-- import Cardano.Ledger.TxIn (TxIn)
-- import Test.Cardano.Ledger.Constrained.TypeRep
-- import Cardano.Ledger.PoolDistr(IndividualPoolStake(..))

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Core (EraRule, PParams)
import Cardano.Ledger.Credential (Credential, Ptr)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair, GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Shelley.LedgerState hiding (deltaReserves, deltaTreasury, rewards)
import qualified Cardano.Ledger.Shelley.LedgerState as LS (deltaReserves, deltaTreasury)
import Cardano.Ledger.Shelley.PoolRank (Likelihood (..), LogWeight (..), NonMyopic (..))
import Cardano.Ledger.UMapCompact (
  RDPair (..),
  Trip (Triple),
  UMap (..),
  compactCoinOrError,
  delView,
  depositView,
  fromCompact,
  ptrView,
  rewView,
  unify,
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Control.State.Transition.Extended (State)
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.VMap (VB, VMap)
import Lens.Micro
import Numeric.Natural (Natural)
import Lens.Micro.Extras (view)

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

iRReservesL :: Lens' (InstantaneousRewards c) (Map (Credential 'Staking c) Integer)
iRReservesL = lens iRReserves (\ds u -> ds {iRReserves = u}) . mapL coinL

iRTreasuryL :: Lens' (InstantaneousRewards c) (Map (Credential 'Staking c) Integer)
iRTreasuryL = lens iRTreasury (\ds u -> ds {iRTreasury = u}) . mapL coinL

deltaResL :: Lens' (InstantaneousRewards c) DeltaCoin
deltaResL = lens LS.deltaReserves (\ds u -> ds {LS.deltaReserves = u})

deltaTreasL :: Lens' (InstantaneousRewards c) DeltaCoin
deltaTreasL = lens LS.deltaTreasury (\ds u -> ds {LS.deltaTreasury = u})

-- ===================================
-- DState

dsUnifiedL :: Lens' (DState c) (UMap c)
dsUnifiedL = lens dsUnified (\ds u -> ds {dsUnified = u})

dsGenDelegsL :: Lens' (DState c) (GenDelegs c)
dsGenDelegsL = lens dsGenDelegs (\ds u -> ds {dsGenDelegs = u})

unGenDelegsL :: Lens' (GenDelegs c) (Map (KeyHash 'Genesis c) (GenDelegPair c))
unGenDelegsL = lens unGenDelegs (\(GenDelegs _) new -> GenDelegs new)

dsIRewardsL :: Lens' (DState c) (InstantaneousRewards c)
dsIRewardsL = lens dsIRewards (\ds u -> ds {dsIRewards = u})

dsFutureGenDelegsL :: Lens' (DState c) (Map (FutureGenDeleg c) (GenDelegPair c))
dsFutureGenDelegsL = lens dsFutureGenDelegs (\ds u -> ds {dsFutureGenDelegs = u})

-- ===================================
-- PState

psStakePoolParamsL :: Lens' (PState c) (Map (KeyHash 'StakePool c) (PoolParams c))
psStakePoolParamsL = lens psStakePoolParams (\ds u -> ds {psStakePoolParams = u})

psFutureStakePoolParamsL :: Lens' (PState c) (Map (KeyHash 'StakePool c) (PoolParams c))
psFutureStakePoolParamsL = lens psFutureStakePoolParams (\ds u -> ds {psFutureStakePoolParams = u})

psRetiringL :: Lens' (PState c) (Map (KeyHash 'StakePool c) EpochNo)
psRetiringL = lens psRetiring (\ds u -> ds {psRetiring = u})

psDepositsL :: Lens' (PState c) (Map (KeyHash 'StakePool c) Coin)
psDepositsL = lens psDeposits (\ds u -> ds {psDeposits = u})

-- ========================================
-- DPState

dpsDStateL :: Lens' (DPState c) (DState c)
dpsDStateL = lens dpsDState (\ds u -> ds {dpsDState = u})

dpsPStateL :: Lens' (DPState c) (PState c)
dpsPStateL = lens dpsPState (\ds u -> ds {dpsPState = u})

-- ========================================
-- UTxOState

utxosUtxoL :: Lens' (UTxOState era) (UTxO era)
utxosUtxoL = lens utxosUtxo (\ds u -> ds {utxosUtxo = u})

utxosDepositedL :: Lens' (UTxOState era) Coin
utxosDepositedL = lens utxosDeposited (\ds u -> ds {utxosDeposited = u})

utxosFeesL :: Lens' (UTxOState era) Coin
utxosFeesL = lens utxosFees (\ds u -> ds {utxosFees = u})

utxosPpupsL :: Lens' (UTxOState era) (State (EraRule "PPUP" era))
utxosPpupsL = lens utxosPpups (\ds u -> ds {utxosPpups = u})

utxosStakeDistrL :: Lens' (UTxOState era) (IncrementalStake (EraCrypto era))
utxosStakeDistrL = lens utxosStakeDistr (\ds u -> ds {utxosStakeDistr = u})

-- =========================================
-- IncrementalStake

isCredMapL :: Lens' (IncrementalStake c) (Map (Credential 'Staking c) Coin)
isCredMapL = lens credMap (\ds u -> ds {credMap = u})

isPtrMapL :: Lens' (IncrementalStake c) (Map Ptr Coin)
isPtrMapL = lens ptrMap (\ds u -> ds {ptrMap = u})

-- =========================================
-- LedgerState

lsUTxOStateL :: Lens' (LedgerState era) (UTxOState era)
lsUTxOStateL = lens lsUTxOState (\ds u -> ds {lsUTxOState = u})

lsDPStateL :: Lens' (LedgerState era) (DPState (EraCrypto era))
lsDPStateL = lens lsDPState (\ds u -> ds {lsDPState = u})

-- ==========================================
-- AccountState

asTreasuryL :: Lens' AccountState Coin
asTreasuryL = lens asTreasury (\ds u -> ds {asTreasury = u})

asReservesL :: Lens' AccountState Coin
asReservesL = lens asReserves (\ds u -> ds {asReserves = u})

-- ===========================================
-- EpochState

esAccountStateL :: Lens' (EpochState era) AccountState
esAccountStateL = lens esAccountState (\ds u -> ds {esAccountState = u})

esSnapshotsL :: Lens' (EpochState era) (SnapShots (EraCrypto era))
esSnapshotsL = lens esSnapshots (\ds u -> ds {esSnapshots = u})

esLStateL :: Lens' (EpochState era) (LedgerState era)
esLStateL = lens esLState (\ds u -> ds {esLState = u})

esPrevPpL :: Lens' (EpochState era) (PParams era)
esPrevPpL = lens esPrevPp (\ds u -> ds {esPrevPp = u})

esPpL :: Lens' (EpochState era) (PParams era)
esPpL = lens esPp (\ds u -> ds {esPp = u})

esNonMyopicL :: Lens' (EpochState era) (NonMyopic (EraCrypto era))
esNonMyopicL = lens esNonMyopic (\ds u -> ds {esNonMyopic = u})

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

-- ===============================================
-- SnapShots

ssStakeMarkL :: Lens' (SnapShots c) (SnapShot c)
ssStakeMarkL = lens ssStakeMark (\ds u -> ds {ssStakeMark = u})

ssStakeMarkPoolDistrL :: Lens' (SnapShots c) (PoolDistr c)
ssStakeMarkPoolDistrL = lens ssStakeMarkPoolDistr (\ds u -> ds {ssStakeMarkPoolDistr = u})

ssStakeSetL :: Lens' (SnapShots c) (SnapShot c)
ssStakeSetL = lens ssStakeSet (\ds u -> ds {ssStakeSet = u})

ssStakeGoL :: Lens' (SnapShots c) (SnapShot c)
ssStakeGoL = lens ssStakeGo (\ds u -> ds {ssStakeGo = u})

ssFeeL :: Lens' (SnapShots c) Coin
ssFeeL = lens ssFee (\ds u -> ds {ssFee = u})

-- =====================================
-- SnapShot

ssStakeL :: Lens' (SnapShot c) (Stake c)
ssStakeL = lens ssStake (\ds u -> ds {ssStake = u})

ssDelegationsL :: Lens' (SnapShot c) (VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c))
ssDelegationsL = lens ssDelegations (\ds u -> ds {ssDelegations = u})

ssPoolParamsL :: Lens' (SnapShot c) (VMap VB VB (KeyHash 'StakePool c) (PoolParams c))
ssPoolParamsL = lens ssPoolParams (\ds u -> ds {ssPoolParams = u})

-- ==========================================
-- NewEpochState

nesPdL :: Lens' (NewEpochState era) (PoolDistr (EraCrypto era))
nesPdL = lens nesPd (\ds u -> ds {nesPd = u})

nesEsL :: Lens' (NewEpochState era) (EpochState era)
nesEsL = lens nesEs (\ds u -> ds {nesEs = u})

unifiedL :: Lens' (NewEpochState era) (UMap (EraCrypto era))
unifiedL = nesEsL . esLStateL . lsDPStateL . dpsDStateL . dsUnifiedL

nesELL :: Lens' (NewEpochState era) EpochNo
nesELL = lens nesEL (\ds u -> ds {nesEL = u})

nesBprevL :: Lens' (NewEpochState era) (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
nesBprevL = lens (unBlocksMade . nesBprev) (\ds u -> ds {nesBprev = BlocksMade u})

nesBcurL :: Lens' (NewEpochState era) (Map (KeyHash 'StakePool (EraCrypto era)) Natural)
nesBcurL = lens (unBlocksMade . nesBcur) (\ds u -> ds {nesBcur = BlocksMade u})

nesRuL :: Lens' (NewEpochState era) (StrictMaybe (PulsingRewUpdate (EraCrypto era)))
nesRuL = lens nesRu (\ds u -> ds {nesRu = u})

nesStashedAVVMAddressesL :: Lens' (NewEpochState era) (StashedAVVMAddresses era)
nesStashedAVVMAddressesL = lens stashedAVVMAddresses (\ds u -> ds {stashedAVVMAddresses = u})

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
  , spRevPtr :: Map (Credential 'Staking c) (Set Ptr)
  , spPtr :: Map Ptr (Credential 'Staking c)
  }

-- | Used to build the abstract view from the map of triples
accumUM :: Split c -> Credential 'Staking c -> Trip c -> Split c
accumUM sp key (Triple (SJust (RDPair r d)) ptrs (SJust p)) =
  sp
    { spRew = Map.insert key (fromCompact r) (spRew sp)
    , spDep = Map.insert key (fromCompact d) (spDep sp)
    , spDel = Map.insert key p (spDel sp)
    , spRevPtr = Map.insertWith (Set.union) key ptrs (spRevPtr sp)
    }
accumUM sp key (Triple SNothing ptrs (SJust p)) =
  sp
    { spDel = Map.insert key p (spDel sp)
    , spRevPtr = Map.insertWith (Set.union) key ptrs (spRevPtr sp)
    }
accumUM sp key (Triple (SJust (RDPair r d)) ptrs SNothing) =
  sp
    { spRew = Map.insert key (fromCompact r) (spRew sp)
    , spDep = Map.insert key (fromCompact d) (spDep sp)
    , spRevPtr = Map.insertWith (Set.union) key ptrs (spRevPtr sp)
    }
accumUM sp key (Triple SNothing ptrs SNothing) =
  sp {spRevPtr = Map.insertWith (Set.union) key ptrs (spRevPtr sp)}

-- | The abstraction function, from concrete (UMap) to abstract (Split)
splitUMap :: UMap c -> Split c
splitUMap (UMap trips ptr) = Map.foldlWithKey' accumUM empty trips
  where
    empty = Split Map.empty Map.empty Map.empty Map.empty ptr

-- | The concretization function from abstract (Split) to concrete (UMap)
unSplitUMap :: Split c -> UMap c
unSplitUMap (Split rew dep deleg _revptr ptr) = unify (merge rew dep) deleg ptr
  where
    merge x y | Map.keysSet x /= Map.keysSet y = error "different domains"
    merge x y = Map.intersectionWith rdpair x y
    rdpair x y = RDPair (compactCoinOrError x) (compactCoinOrError y)

-- Lenses that reach through the concrete  (UMap) using abstract inputs

rewardsUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) Coin)
rewardsUMapL = lens rewView delta
  where
    delta um new = unSplitUMap (split {spRew = new})
      where
        split = splitUMap um

stakeDepositsUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) Coin)
stakeDepositsUMapL = lens depositView delta
  where
    delta um new = unSplitUMap (split {spDep = new})
      where
        split = splitUMap um

ptrsUMapL :: Lens' (UMap c) (Map Ptr (Credential 'Staking c))
ptrsUMapL = lens ptrView (\(UMap x _) p -> UMap x p)

delegationsUMapL :: Lens' (UMap c) (Map (Credential 'Staking c) (KeyHash 'StakePool c))
delegationsUMapL = lens delView delta
  where
    delta um new = unSplitUMap (split {spDel = new})
      where
        split = splitUMap um

coinL :: Lens' Coin Integer
coinL = lens unCoin (\_ y -> Coin y)

-- These lenses might not be safe, use with care
listL :: Lens' a b -> Lens' [a] [b]
listL l = lens (\x -> view l <$> x) (\x y -> zipWith (\ a b -> a & l .~ b) x y)

mapL :: Ord c => Lens' a b -> Lens' (Map c a) (Map c b)
mapL l = lens (\x -> view l <$> x) setter
  where
    setter a b = Map.fromList $ do
      k <- Map.keys b
      let orig = a Map.! k
      let new = b Map.! k
      pure (k, orig & l .~ new)
