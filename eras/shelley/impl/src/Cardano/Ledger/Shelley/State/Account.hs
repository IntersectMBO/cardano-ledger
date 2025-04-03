{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Aeson as Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
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

instance Typeable era => EncCBOR (ShelleyAccountState era) where
  encCBOR sas@(ShelleyAccountState _ _ _ _) =
    let ShelleyAccountState {..} = sas
     in encodeListLen 4
          <> encCBOR sasPtr
          <> encCBOR sasBalance
          <> encCBOR sasDeposit
          <> encCBOR sasStakePoolDelegation

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

instance ToJSON (ShelleyAccountState era) where
  toJSON = object . toShelleyAccountStatePairs
  toEncoding = Aeson.pairs . mconcat . toShelleyAccountStatePairs

toShelleyAccountStatePairs :: Aeson.KeyValue e a => ShelleyAccountState era -> [a]
toShelleyAccountStatePairs sas@(ShelleyAccountState _ _ _ _) =
  let ShelleyAccountState {..} = sas
   in [ "reward" .= sasBalance -- deprecated
      , "balance" .= sasBalance
      , "deposit" .= sasDeposit
      , "ptr" .= sasPtr
      , "spool" .= sasStakePoolDelegation
      ]

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

instance Typeable era => EncCBOR (ShelleyAccounts era) where
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

instance ToJSON (ShelleyAccounts era) where
  toJSON = object . toShelleyAccountsPairs
  toEncoding = Aeson.pairs . mconcat . toShelleyAccountsPairs

toShelleyAccountsPairs :: Aeson.KeyValue e a => ShelleyAccounts era -> [a]
toShelleyAccountsPairs sas@(ShelleyAccounts _ _) =
  let ShelleyAccounts {..} = sas
   in [ "credentials" .= saStates
      , "pointers" .= saPtrs
      ]

instance Default (ShelleyAccounts era) where
  def = ShelleyAccounts mempty mempty

instance EraAccounts ShelleyEra where
  type AccountState ShelleyEra = ShelleyAccountState ShelleyEra
  type Accounts ShelleyEra = ShelleyAccounts ShelleyEra

  accountsMapL = lens saStates $ \sas asMap -> sas {saStates = asMap}

  balanceAccountStateL = lens sasBalance $ \sas b -> sas {sasBalance = b}

  depositAccountStateL = lens sasDeposit $ \sas d -> sas {sasDeposit = d}

  stakePoolDelegationAccountStateL =
    lens (strictMaybeToMaybe . sasStakePoolDelegation) $ \sas d ->
      sas {sasStakePoolDelegation = maybeToStrictMaybe d}

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

  accountsPtrsMapL :: Lens' (Accounts era) (Map Ptr (Credential 'Staking))
  default accountsPtrsMapL ::
    Accounts era ~ ShelleyAccounts era =>
    Lens' (Accounts era) (Map Ptr (Credential 'Staking))
  accountsPtrsMapL = lens saPtrs $ \as ptrsMap -> as {saPtrs = ptrsMap}

  ptrAccountStateL :: Lens' (AccountState era) Ptr
  default ptrAccountStateL ::
    AccountState era ~ ShelleyAccountState era =>
    Lens' (AccountState era) Ptr
  ptrAccountStateL = lens sasPtr $ \as ptr -> as {sasPtr = ptr}

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
registerShelleyAccount cred ptr deposit mStakePool accounts =
  accounts
    & accountsMapL %~ Map.insert cred accountState
    & accountsPtrsMapL %~ Map.insert ptr cred
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
    removePtr accountState = accountsPtrsMapL %~ Map.delete (accountState ^. ptrAccountStateL)
    newAccounts =
      accounts
        & accountsMapL .~ newAccountsMap
        & maybe id removePtr mAccountState
