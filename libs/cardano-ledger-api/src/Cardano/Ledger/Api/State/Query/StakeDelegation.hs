{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Api.State.Query.StakeDelegation (
  -- * Stable query result type
  QueryResultDelegsAndRewards (..),

  -- * Queries
  queryStakePoolDelegsAndRewards,
) where

import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  certDStateL,
  esLStateL,
  lsCertStateL,
  nesEsL,
 )
import Cardano.Ledger.State (
  EraCertState,
  accountsL,
  accountsMapL,
  balanceAccountStateL,
  stakePoolDelegationAccountStateL,
 )
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks)

-- | Stable query result for filtered delegations and reward accounts.
data QueryResultDelegsAndRewards = QueryResultDelegsAndRewards
  { qrdarDelegations :: !(Map (Credential Staking) (KeyHash StakePool))
  , qrdarRewards :: !(Map (Credential Staking) Coin)
  }
  deriving (Show, Eq, Ord, Generic)

deriving instance ToJSON QueryResultDelegsAndRewards

instance NFData QueryResultDelegsAndRewards

instance NoThunks QueryResultDelegsAndRewards

instance EncCBOR QueryResultDelegsAndRewards where
  encCBOR (QueryResultDelegsAndRewards delegations rewards) =
    encode $
      Rec QueryResultDelegsAndRewards
        !> To delegations
        !> To rewards

instance DecCBOR QueryResultDelegsAndRewards where
  decCBOR =
    decode $
      RecD QueryResultDelegsAndRewards
        <! From
        <! From

-- | Query stake pool delegations and reward balances for registered credentials.
-- Source: ouroboros-consensus:ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs:433
--   answerPureBlockQuery case for GetFilteredDelegationsAndRewardAccounts
-- Also: cardano-cli:cardano-cli/src/Cardano/CLI/EraBased/Query/Run.hs:1034
--   CLI invocation (as queryStakeAddresses)
--
-- The two result fields have different coverage: 'qrdarDelegations' contains only
-- credentials that are actually delegated to a stake pool, while 'qrdarRewards'
-- contains all selected registered credentials regardless of delegation status.
--
-- Empty 'Set' returns all registered credentials.
queryStakePoolDelegsAndRewards ::
  EraCertState era =>
  NewEpochState era ->
  Set (Credential Staking) ->
  QueryResultDelegsAndRewards
queryStakePoolDelegsAndRewards nes creds =
  let accountsMap = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL . accountsMapL
      accountsMapFiltered
        | Set.null creds = accountsMap
        | otherwise = accountsMap `Map.restrictKeys` creds
   in QueryResultDelegsAndRewards
        { qrdarDelegations = Map.mapMaybe (^. stakePoolDelegationAccountStateL) accountsMapFiltered
        , qrdarRewards = Map.map (fromCompact . (^. balanceAccountStateL)) accountsMapFiltered
        }
