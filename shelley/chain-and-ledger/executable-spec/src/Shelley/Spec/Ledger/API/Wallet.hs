{-# LANGUAGE DataKinds #-}
module Shelley.Spec.Ledger.API.Wallet
  ( getNonMyopicMemberRewards
  , getUTxO
  , getFilteredUTxO
  )
where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Shelley.Spec.Ledger.Address (Addr (..))
import           Shelley.Spec.Ledger.Credential (Credential (..))
import           Shelley.Spec.Ledger.API.Validation (ShelleyState)
import           Shelley.Spec.Ledger.BaseTypes (Globals (..))
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.EpochBoundary (SnapShot (..), Stake (..), poolStake)
import           Shelley.Spec.Ledger.Keys (KeyHash, KeyRole(..))
import           Shelley.Spec.Ledger.LedgerState (esLState, esNonMyopic, esPp, nesEs, _utxo,
                     _utxoState)
import           Shelley.Spec.Ledger.Rewards (NonMyopic (..), StakeShare (..), getTopRankedPools,
                     nonMyopicMemberRew, nonMyopicStake)
import           Shelley.Spec.Ledger.TxData (PoolParams (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))


-- | Calculate the Non-Myopic Pool Member Rewards for a set of credentials.
-- For each given credential, this function returns a map from each stake
-- pool (identified by the key hash of the pool operator) to the
-- non-myopic pool member reward for that stake pool.
getNonMyopicMemberRewards
  :: Globals
  -> ShelleyState crypto
  -> Set (Credential 'Staking crypto)
  -> Map (Credential 'Staking crypto) (Map (KeyHash 'StakePool crypto) Coin)
getNonMyopicMemberRewards globals ss creds = Map.fromList $
  fmap
    (\cred -> (cred, Map.mapWithKey (mkNMMRewards $ memShare cred) poolData))
    (Set.toList creds)
  where
    total = fromIntegral $ maxLovelaceSupply globals
    toShare (Coin x) = StakeShare (x % total)
    memShare cred = toShare $ Map.findWithDefault (Coin 0) cred (unStake stake)

    es = nesEs ss
    pp = esPp es
    NonMyopic
      { apparentPerformances = aps
      , rewardPot = rPot
      , snap = (SnapShot stake delegs poolParams)
      } = esNonMyopic es

    poolData = Map.intersectionWithKey
                 (\k a p -> (a, p, toShare . sum . unStake $ poolStake k delegs stake))
                 aps
                 poolParams
    topPools = getTopRankedPools rPot (Coin total) pp poolParams aps

    mkNMMRewards ms k (ap, poolp, sigma) = nonMyopicMemberRew pp poolp rPot s ms nmps ap
      where
        s = (toShare . _poolPledge) poolp
        nmps = nonMyopicStake k sigma s pp topPools

-- | Get the full UTxO.
getUTxO
  :: ShelleyState crypto
  -> UTxO crypto
getUTxO = _utxo . _utxoState . esLState . nesEs

-- | Get the UTxO filtered by address.
getFilteredUTxO
  :: ShelleyState crypto
  -> Set (Addr crypto)
  -> UTxO crypto
getFilteredUTxO ss addrs =
    UTxO $ Map.filter (\(TxOut addr _) -> addr `Set.member` addrs) fullUTxO
  where
    UTxO fullUTxO = getUTxO ss
