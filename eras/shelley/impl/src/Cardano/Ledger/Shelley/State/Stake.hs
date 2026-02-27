{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.State.Stake (
  ShelleyInstantStake (..),
  shelleyInstantStakeCredentialsL,
  addShelleyInstantStake,
  deleteShelleyInstantStake,
  resolveShelleyInstantStake,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (
  KeyValuePairs (..),
  NonZero (..),
  ToKeyValuePairs (..),
  unsafeNonZero,
 )
import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  decCBOR,
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.Shelley.State.Account
import Cardano.Ledger.Shelley.TxOut ()
import Cardano.Ledger.State
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..), (.=))
import Data.Coerce
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ShelleyInstantStake era = ShelleyInstantStake
  { sisCredentialStake :: !(Map.Map (Credential Staking) (CompactForm Coin))
  , sisPtrStake :: !(Map.Map Ptr (CompactForm Coin))
  }
  deriving (Generic, Show, Eq, Ord)
  deriving (ToJSON) via KeyValuePairs (ShelleyInstantStake era)

instance NFData (ShelleyInstantStake era)

instance NoThunks (ShelleyInstantStake era)

instance EncCBOR (ShelleyInstantStake era) where
  encCBOR (ShelleyInstantStake credentialStake ptrStake) =
    encodeListLen 2 <> encCBOR credentialStake <> encCBOR ptrStake

instance DecShareCBOR (ShelleyInstantStake era) where
  type Share (ShelleyInstantStake era) = Interns (Credential Staking)
  decShareCBOR credInterns =
    decodeRecordNamed "ShelleyInstantStake" (const 2) $ do
      sisCredentialStake <- decShareCBOR (credInterns, mempty)
      sisPtrStake <- decCBOR
      pure $ ShelleyInstantStake {..}

instance Semigroup (ShelleyInstantStake era) where
  ShelleyInstantStake cs1 ps1 <> ShelleyInstantStake cs2 ps2 =
    ShelleyInstantStake (Map.unionWith (<>) cs1 cs2) (Map.unionWith (<>) ps1 ps2)

instance Monoid (ShelleyInstantStake era) where
  mempty = ShelleyInstantStake Map.empty Map.empty

instance Default (ShelleyInstantStake era) where
  def = mempty

instance ToKeyValuePairs (ShelleyInstantStake era) where
  toKeyValuePairs iStake@(ShelleyInstantStake _ _) =
    let ShelleyInstantStake {..} = iStake -- guard against addition or removal of fields
     in [ "credentials" .= sisCredentialStake
        , "pointers" .= sisPtrStake
        ]

instance EraStake ShelleyEra where
  type InstantStake ShelleyEra = ShelleyInstantStake ShelleyEra
  instantStakeCredentialsL = shelleyInstantStakeCredentialsL
  addInstantStake = addShelleyInstantStake
  deleteInstantStake = deleteShelleyInstantStake
  resolveInstantStake = resolveShelleyInstantStake

shelleyInstantStakeCredentialsL ::
  Lens' (ShelleyInstantStake era) (Map.Map (Credential Staking) (CompactForm Coin))
shelleyInstantStakeCredentialsL = lens sisCredentialStake $ \is m -> is {sisCredentialStake = m}

addShelleyInstantStake ::
  EraTxOut era => UTxO era -> ShelleyInstantStake era -> ShelleyInstantStake era
addShelleyInstantStake = applyUTxOShelleyInstantStake (coerce ((+) @Word64))

deleteShelleyInstantStake ::
  EraTxOut era => UTxO era -> ShelleyInstantStake era -> ShelleyInstantStake era
deleteShelleyInstantStake = applyUTxOShelleyInstantStake (coerce ((-) @Word64))

applyUTxOShelleyInstantStake ::
  EraTxOut era =>
  (CompactForm Coin -> CompactForm Coin -> CompactForm Coin) ->
  UTxO era ->
  ShelleyInstantStake era ->
  ShelleyInstantStake era
applyUTxOShelleyInstantStake f (UTxO u) instantStake =
  Map.foldl' accum instantStake u
  where
    keepOrDeleteCompact new = \case
      Nothing ->
        case new of
          CompactCoin 0 -> Nothing
          final -> Just final
      Just old ->
        case old `f` new of
          CompactCoin 0 -> Nothing
          final -> Just final
    accum ans@(ShelleyInstantStake {sisCredentialStake, sisPtrStake}) out =
      let cc = out ^. compactCoinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefPtr stakingPtr) ->
              ans
                { sisPtrStake = Map.alter (keepOrDeleteCompact cc) stakingPtr sisPtrStake
                }
            Addr _ _ (StakeRefBase stakingKeyHash) ->
              ans
                { sisCredentialStake = Map.alter (keepOrDeleteCompact cc) stakingKeyHash sisCredentialStake
                }
            _other -> ans

-- The invariant in `InstantStake` is that stake is never zero.
resolveShelleyInstantStake ::
  forall era.
  ( EraStake era
  , InstantStake era ~ ShelleyInstantStake era
  , AccountState era ~ ShelleyAccountState era
  , Accounts era ~ ShelleyAccounts era
  ) =>
  ShelleyInstantStake era ->
  ShelleyAccounts era ->
  ActiveStake
resolveShelleyInstantStake instantStake@ShelleyInstantStake {sisPtrStake} sas =
  ActiveStake $ VMap.fromMap $ Map.foldlWithKey' addPtrStake credentialStakeMap sisPtrStake
  where
    !credentialStakeMap = resolveActiveInstantStakeCredentials instantStake sas
    addPtrStake !acc ptr ptrStake = fromMaybe acc $ do
      cred <- Map.lookup ptr (saPtrs sas)
      accountState <- lookupAccountState cred sas
      poolId <- accountState ^. stakePoolDelegationAccountStateL
      let plusPtrStake =
            Just . \case
              -- instant stake is guaranteed to be non-zero due to minUTxO,
              -- therefore usage of 'unsafeNonZero' here is safe.
              Nothing -> StakeWithDelegation (unsafeNonZero ptrStake) poolId
              Just swd -> swd {swdStake = unsafeNonZero $ unNonZero (swdStake swd) <> ptrStake}
      pure $ Map.alter plusPtrStake cred acc
