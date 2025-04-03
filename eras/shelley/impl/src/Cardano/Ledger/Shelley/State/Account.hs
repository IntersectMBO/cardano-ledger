{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.Shelley.State.Account where

import Cardano.Ledger.State hiding (ptrsMap)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (extract)
import Lens.Micro

data ShelleyAccountState era
  = ShelleyAccountState
  { casPtr :: {-# UNPACK #-} !Ptr
  -- ^ Pointer to the certificate in which the stake credential was registered in.
  , casBalance :: {-# UNPACK #-} !(CompactForm Coin)
  -- ^ Current balance of the account
  , casDeposit :: {-# UNPACK #-} !(CompactForm Coin)
  -- ^ Deposit amount that was left when staking credential was registered
  , casStakePoolDelegation :: !(StrictMaybe (KeyHash 'StakePool))
  -- ^ Potential delegation to a stake pool
  }
  deriving (Show, Eq)

data ShelleyAccountsState era = ShelleyAccountsState
  { sasAccountsState :: !(Map (Credential 'Staking) (ShelleyAccountState era))
  -- ^ Map from a staking credential to the account state.
  , sasAccountPtrs :: !(Map Ptr (Credential 'Staking))
  -- ^ A Map from a pointer, to the staking credential. Pointer points to the certificate which
  -- registered the staking credential.
  }
  deriving (Show, Eq)

class EraAccountsState era => ShelleyEraAccountsState era where
  mkShelleyAccountState :: Ptr -> CompactForm Coin -> AccountState era
  default mkShelleyAccountState ::
    AccountState era ~ ShelleyAccountState era =>
    Ptr ->
    CompactForm Coin ->
    AccountState era
  mkShelleyAccountState ptr deposit =
    ShelleyAccountState
      { casPtr = ptr
      , casBalance = mempty
      , casDeposit = deposit
      , casStakePoolDelegation = SNothing
      }

  accountsStatePtrsMapL :: Lens' (AccountsState era) (Map Ptr (Credential 'Staking))
  default accountsStatePtrsMapL ::
    AccountsState era ~ ShelleyAccountsState era =>
    Lens' (AccountsState era) (Map Ptr (Credential 'Staking))
  accountsStatePtrsMapL = lens sasAccountPtrs $ \as ptrsMap -> as {sasAccountPtrs = ptrsMap}

  ptrAccountStateL :: Lens' (AccountState era) Ptr
  default ptrAccountStateL ::
    AccountState era ~ ShelleyAccountState era =>
    Lens' (AccountState era) Ptr
  ptrAccountStateL = lens casPtr $ \as ptr -> as {casPtr = ptr}

registerShelleyStakingCredential ::
  ShelleyEraAccountsState era =>
  Credential 'Staking ->
  -- | Pointer to the certificate that registered the credential
  Ptr ->
  -- | Deposit
  CompactForm Coin ->
  AccountsState era ->
  AccountsState era
registerShelleyStakingCredential cred ptr deposit accountsState =
  accountsState
    & (accountsStateMapL %~ Map.insert cred accountState)
    & (accountsStatePtrsMapL %~ Map.insert ptr cred)
  where
    accountState =
      mkShelleyAccountState ptr deposit

unregisterShelleyStakingCredential ::
  ShelleyEraAccountsState era =>
  Credential 'Staking ->
  AccountsState era ->
  (Maybe (AccountState era), AccountsState era)
unregisterShelleyStakingCredential cred accountsState = (mAccountState, newAccountsState)
  where
    (mAccountState, newAccountsStateMap) = Map.extract cred (accountsState ^. accountsStateMapL)
    removePtr accountState = accountsStatePtrsMapL %~ Map.delete (accountState ^. ptrAccountStateL)
    newAccountsState =
      accountsState
        & (accountsStateMapL .~ newAccountsStateMap)
        & maybe id removePtr mAccountState
