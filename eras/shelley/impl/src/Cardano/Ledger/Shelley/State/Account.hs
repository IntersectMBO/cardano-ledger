{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.State.Account (
  ShelleyAccounts (..),
  ShelleyAccountState (..),
  ShelleyEraAccounts (mkShelleyAccountState, accountsPtrsMapG, ptrAccountStateG),
  shelleyAddAccountState,
  registerShelleyAccount,
  unregisterShelleyAccount,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.State
import Control.DeepSeq (NFData (rnf), deepseq, rwhnf)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Aeson as Aeson (ToJSON (..), (.=))
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (extract)
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ShelleyAccountState era
  = ShelleyAccountState
  { sasPtr :: {-# UNPACK #-} !Ptr
  -- ^ Pointer to the certificate in which the stake credential was registered in.
  , sasBalance :: {-# UNPACK #-} !(CompactForm Coin)
  -- ^ Current balance of the account
  , sasDeposit :: {-# UNPACK #-} !(CompactForm Coin)
  -- ^ Deposit amount that was left when staking credential was registered
  , sasStakePoolDelegation :: !(StrictMaybe (KeyHash 'StakePool))
  -- ^ Potential delegation to a stake pool
  }
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyAccountState era)

instance NFData (ShelleyAccountState era) where
  rnf = rwhnf

instance EncCBOR (ShelleyAccountState era) where
  encCBOR sas@(ShelleyAccountState _ _ _ _) =
    let ShelleyAccountState {..} = sas
     in encodeListLen 4
          <> encCBOR sasPtr
          <> encCBOR sasBalance
          <> encCBOR sasDeposit
          <> encodeNullStrictMaybe encCBOR sasStakePoolDelegation

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
        <*> decodeNullStrictMaybe (interns ks <$> decCBOR)

instance ToKeyValuePairs (ShelleyAccountState era) where
  toKeyValuePairs sas@(ShelleyAccountState _ _ _ _) =
    let ShelleyAccountState {..} = sas
     in [ "reward" .= sasBalance -- deprecated
        , "balance" .= sasBalance
        , "deposit" .= sasDeposit
        , "ptr" .= sasPtr
        , "spool" .= sasStakePoolDelegation
        ]

deriving via KeyValuePairs (ShelleyAccountState era) instance ToJSON (ShelleyAccountState era)

data ShelleyAccounts era = ShelleyAccounts
  { saStates :: !(Map (Credential 'Staking) (ShelleyAccountState era))
  -- ^ Map from a staking credential to the account state.
  , saPtrs :: !(Map Ptr (Credential 'Staking))
  -- ^ A Map from a pointer, to the staking credential. Pointer points to the certificate which
  -- registered the staking credential.
  }
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyAccounts era)

instance NFData (ShelleyAccounts era) where
  rnf (ShelleyAccounts accounts accountPtr) =
    accounts `deepseq` rnf accountPtr

instance EncCBOR (ShelleyAccounts era) where
  encCBOR ShelleyAccounts {saStates, saPtrs} =
    encodeListLen 2 <> encCBOR saStates <> encCBOR saPtrs

instance Typeable era => DecShareCBOR (ShelleyAccounts era) where
  type
    Share (ShelleyAccounts era) =
      (Interns (Credential 'Staking), Interns (KeyHash 'StakePool), Interns (Credential 'DRepRole))
  decSharePlusCBOR =
    StateT
      ( \(a, b, c) ->
          decodeRecordNamed "ShelleyAccounts" (const 2) $ do
            saStates <- decodeMap (interns a <$> decCBOR) (decShareCBOR (b, c))
            let a' = internsFromMap saStates <> a
            saPtrs <- decodeMap decCBOR (interns a' <$> decCBOR)
            pure (ShelleyAccounts {saStates, saPtrs}, (a', b, c))
      )

instance ToKeyValuePairs (ShelleyAccounts era) where
  toKeyValuePairs sa@(ShelleyAccounts _ _) =
    let ShelleyAccounts {..} = sa
     in [ "credentials" .= saStates
        , "pointers" .= saPtrs
        ]

deriving via KeyValuePairs (ShelleyAccounts era) instance ToJSON (ShelleyAccounts era)

instance Default (ShelleyAccounts era) where
  def = ShelleyAccounts mempty mempty

instance EraAccounts ShelleyEra where
  type AccountState ShelleyEra = ShelleyAccountState ShelleyEra
  type Accounts ShelleyEra = ShelleyAccounts ShelleyEra

  addAccountState = shelleyAddAccountState

  accountsMapL = lens saStates $ \sas asMap -> sas {saStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . sasStakePoolDelegation) $ \sas d ->
      sas {sasStakePoolDelegation = maybeToStrictMaybe d}

  unregisterAccount = unregisterShelleyAccount

instance ShelleyEraAccounts ShelleyEra

class EraAccounts era => ShelleyEraAccounts era where
  mkShelleyAccountState :: Ptr -> CompactForm Coin -> AccountState era
  default mkShelleyAccountState ::
    AccountState era ~ ShelleyAccountState era =>
    Ptr ->
    CompactForm Coin ->
    AccountState era
  mkShelleyAccountState ptr deposit =
    ShelleyAccountState
      { sasPtr = ptr
      , sasBalance = mempty
      , sasDeposit = deposit
      , sasStakePoolDelegation = SNothing
      }

  -- | This lens is explicitely not exported, since it is not safe to overwrite pointers
  -- directly. For accessing this Map use `accountsPtrsMapG` instead.
  accountsPtrsMapL :: Lens' (Accounts era) (Map Ptr (Credential 'Staking))
  default accountsPtrsMapL ::
    Accounts era ~ ShelleyAccounts era =>
    Lens' (Accounts era) (Map Ptr (Credential 'Staking))
  accountsPtrsMapL = lens saPtrs $ \as ptrsMap -> as {saPtrs = ptrsMap}

  -- | Get the map with all of the pointers and their respective credentials.
  accountsPtrsMapG :: SimpleGetter (Accounts era) (Map Ptr (Credential 'Staking))
  accountsPtrsMapG = accountsPtrsMapL

  -- | This is a getter for a `Ptr`. It is not a full lens, because it is not only unsafe to modify
  -- a pointer for an existing AccountState due to violation of an invariant in the
  -- `ShelleyAccounts`, but also because once account is registered pointer cannot change. Pointer
  -- describes unique point on chain when registration has occured, which means it can't change.
  ptrAccountStateG :: SimpleGetter (AccountState era) Ptr
  default ptrAccountStateG ::
    AccountState era ~ ShelleyAccountState era =>
    SimpleGetter (AccountState era) Ptr
  ptrAccountStateG = to sasPtr

shelleyAddAccountState ::
  ShelleyEraAccounts era =>
  Credential Staking ->
  AccountState era ->
  Accounts era ->
  Accounts era
shelleyAddAccountState cred accountState accounts =
  accounts
    & accountsMapL %~ Map.insert cred accountState
    & accountsPtrsMapL %~ Map.insert (accountState ^. ptrAccountStateG) cred

registerShelleyAccount ::
  ShelleyEraAccounts era =>
  Credential 'Staking ->
  -- | Pointer to the certificate that registered the credential
  Ptr ->
  -- | Deposit
  CompactForm Coin ->
  Maybe (KeyHash 'StakePool) ->
  Accounts era ->
  Accounts era
registerShelleyAccount cred ptr deposit mStakePool = addAccountState cred accountState
  where
    accountState =
      mkShelleyAccountState ptr deposit & stakePoolDelegationAccountStateL .~ mStakePool

unregisterShelleyAccount ::
  ShelleyEraAccounts era =>
  -- | Credential to unregister
  Credential 'Staking ->
  -- | `Accounts` to remove the account state from
  Accounts era ->
  -- | Returns `Just` whenever account was registered and `Nothing` otherwise. Produced `Accounts`
  -- will have the account state removed, if it was present there to begin with.
  (Maybe (AccountState era), Accounts era)
unregisterShelleyAccount cred accounts = (mAccountState, newAccounts)
  where
    (mAccountState, newAccountsMap) = Map.extract cred (accounts ^. accountsMapL)
    removePtr accountState = accountsPtrsMapL %~ Map.delete (accountState ^. ptrAccountStateG)
    newAccounts =
      accounts
        & accountsMapL .~ newAccountsMap
        & maybe id removePtr mAccountState
