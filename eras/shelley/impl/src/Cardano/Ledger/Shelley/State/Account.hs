{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.State.Account where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.State hiding (balance)
import Control.DeepSeq (NFData (rnf), deepseq, rwhnf)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (extract)
import Data.Typeable
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

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
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyAccountState era)

instance NFData (ShelleyAccountState era) where
  rnf = rwhnf

instance Typeable era => EncCBOR (ShelleyAccountState era) where
  encCBOR (ShelleyAccountState ptr balance deposit delegation) =
    encodeListLen 4 <> encCBOR ptr <> encCBOR balance <> encCBOR deposit <> encCBOR delegation

instance Typeable era => DecShareCBOR (ShelleyAccountState era) where
  type
    Share (ShelleyAccountState era) =
      (Interns (KeyHash 'StakePool), Interns (Credential 'DRepRole))
  decShareCBOR (ks, _) =
    decodeRecordNamed "ShelleyAccountState" (const 4) $
      ShelleyAccountState
        <$> decCBOR
        <*> decCBOR
        <*> decCBOR
        <*> decodeStrictMaybe (interns ks <$> decCBOR)

data ShelleyAccountsState era = ShelleyAccountsState
  { sasAccountStates :: !(Map (Credential 'Staking) (ShelleyAccountState era))
  -- ^ Map from a staking credential to the account state.
  , sasAccountPtrs :: !(Map Ptr (Credential 'Staking))
  -- ^ A Map from a pointer, to the staking credential. Pointer points to the certificate which
  -- registered the staking credential.
  }
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyAccountsState era)

instance NFData (ShelleyAccountsState era) where
  rnf (ShelleyAccountsState accountsState accountPtr) =
    accountsState `deepseq` rnf accountPtr

instance Typeable era => EncCBOR (ShelleyAccountsState era) where
  encCBOR ShelleyAccountsState {sasAccountStates, sasAccountPtrs} =
    encodeListLen 2 <> encCBOR sasAccountStates <> encCBOR sasAccountPtrs

instance Typeable era => DecShareCBOR (ShelleyAccountsState era) where
  type
    Share (ShelleyAccountsState era) =
      (Interns (Credential 'Staking), Interns (KeyHash 'StakePool), Interns (Credential 'DRepRole))
  decSharePlusCBOR =
    StateT
      ( \(a, b, c) ->
          decodeRecordNamed "ShelleyAccountsState" (const 2) $ do
            sasAccountStates <- decodeMap (interns a <$> decCBOR) (decShareCBOR (b, c))
            let a' = internsFromMap sasAccountStates <> a
            sasAccountPtrs <- decodeMap decCBOR (interns a' <$> decCBOR)
            pure (ShelleyAccountsState {sasAccountStates, sasAccountPtrs}, (a', b, c))
      )

instance Default (ShelleyAccountsState era) where
  def = ShelleyAccountsState mempty mempty

instance EraAccountsState ShelleyEra where
  type AccountState ShelleyEra = ShelleyAccountState ShelleyEra
  type AccountsState ShelleyEra = ShelleyAccountsState ShelleyEra

  accountsStateMapL = lens sasAccountStates $ \sas asMap -> sas {sasAccountStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . casStakePoolDelegation) $ \sas d -> sas {casStakePoolDelegation = d}

instance ShelleyEraAccountsState ShelleyEra

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
