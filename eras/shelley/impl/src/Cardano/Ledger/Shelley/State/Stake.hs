{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
)
where

import Cardano.Ledger.Address
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
import Cardano.Ledger.Shelley.TxOut ()
import Cardano.Ledger.State
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Coerce
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Typeable
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ShelleyInstantStake era = ShelleyInstantStake
  { sisCredentialStake :: !(Map.Map (Credential 'Staking) (CompactForm Coin))
  , sisPtrStake :: !(Map.Map Ptr (CompactForm Coin))
  }
  deriving (Generic, Show, Eq, Ord)

instance NFData (ShelleyInstantStake era)
instance NoThunks (ShelleyInstantStake era)

instance Typeable era => EncCBOR (ShelleyInstantStake era) where
  encCBOR (ShelleyInstantStake credentialStake ptrStake) =
    encodeListLen 2 <> encCBOR credentialStake <> encCBOR ptrStake

instance DecShareCBOR (ShelleyInstantStake era) where
  type Share (ShelleyInstantStake era) = Interns (Credential 'Staking)
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

instance ToJSON (ShelleyInstantStake era) where
  toJSON = object . toIncrementalStakePairs
  toEncoding = pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs :: KeyValue e a => ShelleyInstantStake era -> [a]
toIncrementalStakePairs iStake@(ShelleyInstantStake _ _) =
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
  Lens' (ShelleyInstantStake era) (Map.Map (Credential 'Staking) (CompactForm Coin))
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
  (EraStake era, InstantStake era ~ ShelleyInstantStake era) =>
  ShelleyInstantStake era -> UM.UMap -> Stake
resolveShelleyInstantStake instantStake@ShelleyInstantStake {sisPtrStake} umap@(UM.UMap triplesMap ptrsMap) =
  Stake $ VMap.fromMap $ Map.foldlWithKey' addPtrStake credentialStakeMap sisPtrStake
  where
    !credentialStakeMap = resolveActiveInstantStakeCredentials instantStake umap
    addPtrStake !acc ptr ptrStake = fromMaybe acc $ do
      cred <- Map.lookup ptr ptrsMap
      -- Ensure only active staking credential receive Ptr delegations
      _ <- UM.umElemRDActive =<< Map.lookup cred triplesMap
      let plusPtrStake =
            Just . \case
              Nothing -> ptrStake
              Just curStake -> curStake <> ptrStake
      -- instant stake is guaranteed to be non-zero due to minUTxO, so no need to guard against mempty
      pure $! Map.alter plusPtrStake cred acc
