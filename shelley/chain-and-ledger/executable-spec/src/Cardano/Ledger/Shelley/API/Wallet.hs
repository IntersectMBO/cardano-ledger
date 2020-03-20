module Cardano.Ledger.Shelley.API.Wallet
  ( getNonMyopicMemberRewards
  )
where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Cardano.Ledger.Shelley.API.Validation (ShelleyState)
import           Shelley.Spec.Ledger.BaseTypes (Globals (..))
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.EpochBoundary (SnapShot (..), Stake (..), poolStake)
import           Shelley.Spec.Ledger.Keys (KeyHash)
import           Shelley.Spec.Ledger.LedgerState (esNonMyopic, esPp, nesEs)
import           Shelley.Spec.Ledger.Rewards (NonMyopic (..), StakeShare (..), getTopRankedPools,
                     nonMyopicMemberRew, nonMyopicStake)
import           Shelley.Spec.Ledger.TxData (Credential (..), PoolParams (..))


-- | Calculate the Non-Myopic Pool Member Rewards for a set of credentials.
-- For each given credential, this function returns a map from each stake
-- pool (identified by the key hash of the pool operator) to the
-- non-myopic pool member reward for that stake pool.
getNonMyopicMemberRewards
  :: Globals
  -> ShelleyState crypto
  -> Set (Credential crypto)
  -> Map (Credential crypto) (Map (KeyHash crypto) Coin)
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
