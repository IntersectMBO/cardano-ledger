{-# LANGUAGE DataKinds #-}

module Shelley.Spec.Ledger.API.Wallet
  ( getNonMyopicMemberRewards,
    getUTxO,
    getFilteredUTxO,
    getLeaderSchedule,
    getTotalStake,
  )
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto (VRF)
import Cardano.Ledger.Era (Crypto, Era)
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
import Shelley.Spec.Ledger.Delegation.Certificates (IndividualPoolStake (..), unPoolDistr)
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (..), SignKeyVRF)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState (..),
    DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
    stakeDistr,
  )
import Shelley.Spec.Ledger.OverlaySchedule (isOverlaySlot)
import Shelley.Spec.Ledger.Rewards
  ( NonMyopic (..),
    StakeShare (..),
    getTopRankedPools,
    nonMyopicMemberRew,
    nonMyopicStake,
    percentile',
  )
import Shelley.Spec.Ledger.STS.Tickn (TicknState (..))
import Shelley.Spec.Ledger.TxBody (PoolParams (..), TxOut (..))
import Shelley.Spec.Ledger.UTxO (UTxO (..))
import qualified Cardano.Ledger.Val as Val

-- | Calculate the current total stake.
getTotalStake :: Globals -> ShelleyState era -> Coin
getTotalStake globals ss =
  let supply = Coin . fromIntegral $ maxLovelaceSupply globals
      EpochState acnt _ _ _ _ _ = nesEs ss
   in supply Val.~~ (_reserves acnt)

-- | Calculate the Non-Myopic Pool Member Rewards for a set of credentials.
-- For each given credential, this function returns a map from each stake
-- pool (identified by the key hash of the pool operator) to the
-- non-myopic pool member reward for that stake pool.
getNonMyopicMemberRewards ::
  Era era =>
  Globals ->
  ShelleyState era ->
  Set (Either Coin (Credential 'Staking era)) ->
  Map (Either Coin (Credential 'Staking era)) (Map (KeyHash 'StakePool era) Coin)
getNonMyopicMemberRewards globals ss creds =
  Map.fromList $
    fmap
      (\cred -> (cred, Map.mapWithKey (mkNMMRewards $ memShare cred) poolData))
      (Set.toList creds)
  where
    total = fromIntegral $ maxLovelaceSupply globals
    toShare (Coin x) = StakeShare (x % total)
    memShare (Right cred) = toShare $ Map.findWithDefault (Coin 0) cred (EB.unStake stake)
    memShare (Left coin) = toShare coin
    es = nesEs ss
    pp = esPp es
    NonMyopic
      { likelihoodsNM = ls,
        rewardPotNM = rPot
      } = esNonMyopic es
    utxo = _utxo . _utxoState . esLState $ es
    dstate = _dstate . _delegationState . esLState $ es
    pstate = _pstate . _delegationState . esLState $ es
    EB.SnapShot stake delegs poolParams = stakeDistr utxo dstate pstate
    poolData =
      Map.mapWithKey
        (\k p -> (percentile' (histLookup k), p, toShare . fold . EB.unStake $ EB.poolStake k delegs stake))
        poolParams
    histLookup k = fromMaybe mempty (Map.lookup k ls)
    topPools = getTopRankedPools rPot (Coin total) pp poolParams (fmap percentile' ls)
    mkNMMRewards ms k (ap, poolp, sigma) =
      if checkPledge poolp
        then nonMyopicMemberRew pp poolp rPot s ms nmps ap
        else mempty
      where
        s = (toShare . _poolPledge) poolp
        nmps = nonMyopicStake k sigma s pp topPools
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
  ChainDepState era ->
  KeyHash 'StakePool era ->
  SignKeyVRF era ->
  Set SlotNo
getLeaderSchedule globals ss cds poolHash key = Set.filter isLeader epochSlots
  where
    isLeader slotNo =
      let y = VRF.evalCertified () (mkSeed seedL slotNo epochNonce) key
       in not (isOverlaySlot slotNo overlaySched)
            && checkLeaderValue (VRF.certifiedOutput y) stake f
    stake = maybe 0 individualPoolStake $ Map.lookup poolHash poolDistr
    overlaySched = nesOsched ss
    poolDistr = unPoolDistr $ nesPd ss
    TicknState epochNonce _ = csTickn cds
    currentEpoch = nesEL ss
    ei = epochInfo globals
    f = activeSlotCoeff globals
    epochSlots = Set.fromList [a .. b]
      where
        (a, b) = runIdentity $ epochInfoRange ei currentEpoch
