{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- `unused-pattern-binds` warning is disabled to preserve safety of `RecordWildCards` "trick" while
-- avoiding unnecessary pattern matching that is not zero cost with pattern synonyms.
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-pattern-binds #-}

module Cardano.Ledger.Conway.State.Account (
  ConwayAccountState (
    ConwayAccountState,
    casBalance,
    casDeposit,
    casStakePoolDelegation,
    casDRepDelegation
  ),
  balanceConwayAccountStateL,
  depositConwayAccountStateL,
  stakePoolDelegationConwayAccountStateL,
  dRepDelegationConwayAccountStateL,
  ConwayAccounts (..),
  ConwayEraAccounts (..),
  accountStateDelegatee,
  registerConwayAccount,
  unregisterConwayAccount,
  lookupDRepDelegation,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Cardano.Ledger.State
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Aeson (ToJSON (..), (.=))
import Data.Default
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (extract)
import Data.Typeable
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ConwayAccountState era
  = CASNoDelegation
      -- | Current balance of the account
      {-# UNPACK #-} !(CompactForm Coin)
      -- | Deposit amount that was left when staking credential was registered
      {-# UNPACK #-} !(CompactForm Coin)
  | CASStakePool
      -- | Current balance of the account
      {-# UNPACK #-} !(CompactForm Coin)
      -- | Deposit amount that was left when staking credential was registered
      {-# UNPACK #-} !(CompactForm Coin)
      -- | Delegation to a stake pool
      !(KeyHash StakePool)
  | CASDRep
      -- | Current balance of the account
      {-# UNPACK #-} !(CompactForm Coin)
      -- | Deposit amount that was left when staking credential was registered
      {-# UNPACK #-} !(CompactForm Coin)
      -- | Delegation to a DRep
      !DRep
  | CASStakePoolAndDRep
      -- | Current balance of the account
      {-# UNPACK #-} !(CompactForm Coin)
      {-# UNPACK #-} !(CompactForm Coin)
      -- | Delegation to a stake pool
      -- ^ Deposit amount that was left when staking credential was registered
      !(KeyHash StakePool)
      -- | Delegation to a DRep
      !DRep
  deriving (Show, Eq, Generic)

viewConwayAccountState ::
  ConwayAccountState era ->
  (CompactForm Coin, CompactForm Coin, Maybe (KeyHash StakePool), Maybe DRep)
viewConwayAccountState (CASNoDelegation x y) = (x, y, Nothing, Nothing)
viewConwayAccountState (CASStakePool x y z) = (x, y, Just z, Nothing)
viewConwayAccountState (CASDRep x y w) = (x, y, Nothing, Just w)
viewConwayAccountState (CASStakePoolAndDRep x y z w) = (x, y, Just z, Just w)

pattern ConwayAccountState ::
  CompactForm Coin ->
  CompactForm Coin ->
  Maybe (KeyHash StakePool) ->
  Maybe DRep ->
  ConwayAccountState era
pattern ConwayAccountState
  { casBalance
  , casDeposit
  , casStakePoolDelegation
  , casDRepDelegation
  } <-
  (viewConwayAccountState -> (casBalance, casDeposit, casStakePoolDelegation, casDRepDelegation))
  where
    ConwayAccountState x y Nothing Nothing = CASNoDelegation x y
    ConwayAccountState x y (Just z) Nothing = CASStakePool x y z
    ConwayAccountState x y Nothing (Just w) = CASDRep x y w
    ConwayAccountState x y (Just z) (Just w) = CASStakePoolAndDRep x y z w

{-# COMPLETE ConwayAccountState #-}

{-# INLINE ConwayAccountState #-}

instance NoThunks (ConwayAccountState era)

instance NFData (ConwayAccountState era) where
  rnf = rwhnf

instance EncCBOR (ConwayAccountState era) where
  encCBOR cas@ConwayAccountState {..} =
    let ConwayAccountState _ _ _ _ = cas
     in encodeListLen 4
          <> encCBOR casBalance
          <> encCBOR casDeposit
          <> encodeNullMaybe encCBOR casStakePoolDelegation
          <> encodeNullMaybe encCBOR casDRepDelegation

instance Typeable era => DecShareCBOR (ConwayAccountState era) where
  type
    Share (ConwayAccountState era) =
      (Interns (KeyHash StakePool), Interns (Credential DRepRole))
  decShareCBOR (ks, cd) =
    decodeRecordNamed "ConwayAccountState" (const 4) $
      ConwayAccountState
        <$> decCBOR
        <*> decCBOR
        <*> decodeNullMaybe (interns ks <$> decCBOR)
        <*> decodeNullMaybe (decShareCBOR cd)

instance ToKeyValuePairs (ConwayAccountState era) where
  toKeyValuePairs cas@ConwayAccountState {..} =
    let ConwayAccountState _ _ _ _ = cas
     in [ "reward" .= casBalance -- deprecated
        , "balance" .= casBalance
        , "deposit" .= casDeposit
        , "spool" .= casStakePoolDelegation
        , "drep" .= casDRepDelegation
        ]

deriving via KeyValuePairs (ConwayAccountState era) instance ToJSON (ConwayAccountState era)

newtype ConwayAccounts era = ConwayAccounts
  { caStates :: Map (Credential Staking) (ConwayAccountState era)
  -- ^ Map from a staking credential to the account state.
  }
  deriving (Show, Eq, Generic, EncCBOR, NoThunks, NFData, Default, ToJSON)

instance Typeable era => DecShareCBOR (ConwayAccounts era) where
  type
    Share (ConwayAccounts era) =
      (Interns (Credential Staking), Interns (KeyHash StakePool), Interns (Credential DRepRole))
  decSharePlusCBOR =
    StateT $ \(a, b, c) -> do
      caStates <- decodeMap (interns a <$> decCBOR) (decShareCBOR (b, c))
      let a' = internsFromMap caStates <> a
      pure (ConwayAccounts {caStates}, (a', b, c))

instance EraAccounts ConwayEra where
  type AccountState ConwayEra = ConwayAccountState ConwayEra
  type Accounts ConwayEra = ConwayAccounts ConwayEra

  addAccountState cred accountState = accountsMapL %~ Map.insert cred accountState

  accountsMapL = lens caStates $ \cas asMap -> cas {caStates = asMap}

  balanceAccountStateL = balanceConwayAccountStateL
  {-# INLINE balanceAccountStateL #-}

  depositAccountStateL = depositConwayAccountStateL
  {-# INLINE depositAccountStateL #-}

  stakePoolDelegationAccountStateL = stakePoolDelegationConwayAccountStateL
  {-# INLINE stakePoolDelegationAccountStateL #-}

  unregisterAccount = unregisterConwayAccount

-- /Note/ - Lenses below do not use pattern synonym in order to guarantee optimal performance

balanceConwayAccountStateL :: Lens' (ConwayAccountState era) (CompactForm Coin)
balanceConwayAccountStateL =
  lens
    ( \case
        CASNoDelegation balance _ -> balance
        CASStakePool balance _ _ -> balance
        CASDRep balance _ _ -> balance
        CASStakePoolAndDRep balance _ _ _ -> balance
    )
    $ \cas balance ->
      case cas of
        CASNoDelegation _ deposit -> CASNoDelegation balance deposit
        CASStakePool _ deposit stakePool -> CASStakePool balance deposit stakePool
        CASDRep _ deposit dRep -> CASDRep balance deposit dRep
        CASStakePoolAndDRep _ deposit stakePool dRep -> CASStakePoolAndDRep balance deposit stakePool dRep
{-# INLINE balanceConwayAccountStateL #-}

depositConwayAccountStateL :: Lens' (ConwayAccountState era) (CompactForm Coin)
depositConwayAccountStateL =
  lens
    ( \case
        CASNoDelegation _ deposit -> deposit
        CASStakePool _ deposit _ -> deposit
        CASDRep _ deposit _ -> deposit
        CASStakePoolAndDRep _ deposit _ _ -> deposit
    )
    $ \cas deposit ->
      case cas of
        CASNoDelegation balance _ -> CASNoDelegation balance deposit
        CASStakePool balance _ stakePool -> CASStakePool balance deposit stakePool
        CASDRep balance _ dRep -> CASDRep balance deposit dRep
        CASStakePoolAndDRep balance _ stakePool dRep -> CASStakePoolAndDRep balance deposit stakePool dRep
{-# INLINE depositConwayAccountStateL #-}

stakePoolDelegationConwayAccountStateL :: Lens' (ConwayAccountState era) (Maybe (KeyHash StakePool))
stakePoolDelegationConwayAccountStateL =
  lens
    ( \case
        CASNoDelegation _ _ -> Nothing
        CASStakePool _ _ stakePool -> Just stakePool
        CASDRep _ _ _ -> Nothing
        CASStakePoolAndDRep _ _ stakePool _ -> Just stakePool
    )
    $ \cas mStakePool ->
      case cas of
        CASNoDelegation balance deposit
          | Just stakePool <- mStakePool -> CASStakePool balance deposit stakePool
          | otherwise -> CASNoDelegation balance deposit
        CASStakePool balance deposit _
          | Just stakePool <- mStakePool -> CASStakePool balance deposit stakePool
          | otherwise -> CASNoDelegation balance deposit
        CASDRep balance deposit dRep
          | Just stakePool <- mStakePool -> CASStakePoolAndDRep balance deposit stakePool dRep
          | otherwise -> CASDRep balance deposit dRep
        CASStakePoolAndDRep balance deposit _ dRep
          | Just stakePool <- mStakePool -> CASStakePoolAndDRep balance deposit stakePool dRep
          | otherwise -> CASDRep balance deposit dRep
{-# INLINE stakePoolDelegationConwayAccountStateL #-}

dRepDelegationConwayAccountStateL :: Lens' (ConwayAccountState era) (Maybe DRep)
dRepDelegationConwayAccountStateL =
  lens
    ( \case
        CASNoDelegation _ _ -> Nothing
        CASStakePool _ _ _ -> Nothing
        CASDRep _ _ dRep -> Just dRep
        CASStakePoolAndDRep _ _ _ dRep -> Just dRep
    )
    $ \cas mDRep ->
      case cas of
        CASNoDelegation balance deposit
          | Just dRep <- mDRep -> CASDRep balance deposit dRep
          | otherwise -> CASNoDelegation balance deposit
        CASStakePool balance deposit stakePool
          | Just dRep <- mDRep -> CASStakePoolAndDRep balance deposit stakePool dRep
          | otherwise -> CASStakePool balance deposit stakePool
        CASDRep balance deposit _
          | Just dRep <- mDRep -> CASDRep balance deposit dRep
          | otherwise -> CASNoDelegation balance deposit
        CASStakePoolAndDRep balance deposit stakePool _
          | Just dRep <- mDRep -> CASStakePoolAndDRep balance deposit stakePool dRep
          | otherwise -> CASStakePool balance deposit stakePool
{-# INLINE dRepDelegationConwayAccountStateL #-}

class EraAccounts era => ConwayEraAccounts era where
  mkConwayAccountState :: CompactForm Coin -> AccountState era
  default mkConwayAccountState ::
    AccountState era ~ ConwayAccountState era =>
    CompactForm Coin ->
    AccountState era
  mkConwayAccountState deposit =
    ConwayAccountState
      { casBalance = mempty
      , casDeposit = deposit
      , casStakePoolDelegation = Nothing
      , casDRepDelegation = Nothing
      }

  dRepDelegationAccountStateL :: Lens' (AccountState era) (Maybe DRep)

instance ConwayEraAccounts ConwayEra where
  dRepDelegationAccountStateL = dRepDelegationConwayAccountStateL
  {-# INLINE dRepDelegationAccountStateL #-}

lookupDRepDelegation :: ConwayEraAccounts era => Credential Staking -> Accounts era -> Maybe DRep
lookupDRepDelegation cred accounts = do
  accountState <- lookupAccountState cred accounts
  accountState ^. dRepDelegationAccountStateL

registerConwayAccount ::
  ConwayEraAccounts era =>
  Credential Staking ->
  -- | Deposit
  CompactForm Coin ->
  Maybe Delegatee ->
  Accounts era ->
  Accounts era
registerConwayAccount cred deposit mDelegatee accounts =
  accounts
    & accountsMapL %~ Map.insert cred accountState
  where
    accountState =
      case mDelegatee of
        Nothing -> mkConwayAccountState deposit
        Just delegatee ->
          mkConwayAccountState deposit
            & stakePoolDelegationAccountStateL .~ getStakePoolDelegatee delegatee
            & dRepDelegationAccountStateL .~ getDRepDelegatee delegatee

unregisterConwayAccount ::
  EraAccounts era =>
  -- | Credential to unregister
  Credential Staking ->
  -- | `Accounts` to remove the account state from
  Accounts era ->
  -- | Returns `Just` whenever account was registered and `Nothing` otherwise. Produced `Accounts`
  -- will have the account state removed, if it was present there to begin with.
  (Maybe (AccountState era), Accounts era)
unregisterConwayAccount cred accounts = (mAccountState, newAccounts)
  where
    (mAccountState, newAccountsMap) = Map.extract cred (accounts ^. accountsMapL)
    newAccounts = accounts & accountsMapL .~ newAccountsMap

accountStateDelegatee :: ConwayEraAccounts era => AccountState era -> Maybe Delegatee
accountStateDelegatee accountState =
  mkDelegatee
    (accountState ^. stakePoolDelegationAccountStateL)
    (accountState ^. dRepDelegationAccountStateL)
