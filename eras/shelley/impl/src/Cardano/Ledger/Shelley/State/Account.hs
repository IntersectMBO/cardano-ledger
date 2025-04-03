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

data ShelleyAccountStates era = ShelleyAccountStates
  { sasAccountStates :: !(Map (Credential 'Staking) (ShelleyAccountState era))
  -- ^ Map from a staking credential to the account state.
  , sasAccountPtrs :: !(Map Ptr (Credential 'Staking))
  -- ^ A Map from a pointer, to the staking credential. Pointer points to the certificate which
  -- registered the staking credential.
  }
  deriving (Show, Eq)

class EraAccountState era => ShelleyEraAccountState era where
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

  accountStatesPtrsMapL :: Lens' (AccountStates era) (Map Ptr (Credential 'Staking))
  default accountStatesPtrsMapL ::
    AccountStates era ~ ShelleyAccountStates era =>
    Lens' (AccountStates era) (Map Ptr (Credential 'Staking))
  accountStatesPtrsMapL = lens sasAccountPtrs $ \as ptrsMap -> as {sasAccountPtrs = ptrsMap}

  ptrAccountStateL :: Lens' (AccountState era) Ptr
  default ptrAccountStateL ::
    AccountState era ~ ShelleyAccountState era =>
    Lens' (AccountState era) Ptr
  ptrAccountStateL = lens casPtr $ \as ptr -> as {casPtr = ptr}

registerShelleyStakingCredential ::
  ShelleyEraAccountState era =>
  Credential 'Staking ->
  -- | Pointer to the certificate that registered the credential
  Ptr ->
  -- | Deposit
  CompactForm Coin ->
  AccountStates era ->
  AccountStates era
registerShelleyStakingCredential cred ptr deposit accountStates =
  accountStates
    & (accountStatesMapL %~ Map.insert cred accountState)
    & (accountStatesPtrsMapL %~ Map.insert ptr cred)
  where
    accountState =
      mkShelleyAccountState ptr deposit

unregisterShelleyStakingCredential ::
  ShelleyEraAccountState era =>
  Credential 'Staking ->
  AccountStates era ->
  (Maybe (AccountState era), AccountStates era)
unregisterShelleyStakingCredential cred accountStates = (mAccountState, newAccountStates)
  where
    (mAccountState, newAccountStatesMap) = Map.extract cred (accountStates ^. accountStatesMapL)
    removePtr accountState = accountStatesPtrsMapL %~ Map.delete (accountState ^. ptrAccountStateL)
    newAccountStates =
      accountStates
        & (accountStatesMapL .~ newAccountStatesMap)
        & maybe id removePtr mAccountState
