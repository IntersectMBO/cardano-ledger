{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.State.Stake (
  ShelleyInstantStake (..),
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
import Control.Monad (guard)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Coerce
import Data.Default (Default (..))
import Data.Functor.Identity
import qualified Data.Map.Merge.Strict as Map
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
  addInstantStake = addShelleyInstantStake
  deleteInstantStake = deleteShelleyInstantStake
  resolveInstantStake = resolveShelleyInstantStake

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
applyUTxOShelleyInstantStake f (UTxO u) instantInstantStake =
  Map.foldl' accum instantInstantStake u
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
              ShelleyInstantStake
                { sisCredentialStake
                , sisPtrStake = Map.alter (keepOrDeleteCompact cc) stakingPtr sisPtrStake
                }
            Addr _ _ (StakeRefBase stakingKeyHash) ->
              ShelleyInstantStake
                { sisCredentialStake = Map.alter (keepOrDeleteCompact cc) stakingKeyHash sisCredentialStake
                , sisPtrStake
                }
            _other -> ans

-- The invariant in `InstantStake` is that stake is never zero.
resolveShelleyInstantStake :: ShelleyInstantStake era -> UM.UMap -> Stake
resolveShelleyInstantStake ShelleyInstantStake {sisCredentialStake, sisPtrStake} (UM.UMap triplesMap ptrsMap) =
  Stake $ VMap.fromMap $ Map.foldlWithKey' addPtrStake credentialStakeMap sisPtrStake
  where
    -- Retain any non-zero reward
    getReward umElem = do
      rd <- UM.umElemRDActive umElem
      let reward = UM.rdReward rd
      reward <$ guard (reward > mempty)
    addStake _ stake umElem = Identity $ maybe stake ((stake <>) . UM.rdReward) (UM.umElemRDActive umElem)
    -- This is the total stake including the rewards, but ignoring all the stake coming ftom the pointers
    credentialStakeMap =
      Map.merge
        Map.dropMissing -- ignore non-registered stake credentials
        (Map.mapMaybeMissing (const getReward)) -- use the reward amount, unless it is zero
        (Map.zipWithAMatched addStake) -- combine the stake with the reward amount
        sisCredentialStake
        triplesMap
    addPtrStake acc ptr ptrStake = fromMaybe acc $ do
      cred <- Map.lookup ptr ptrsMap
      let plusPtrStake =
            Just . \case
              Nothing -> ptrStake
              Just curStake -> curStake <> ptrStake
      pure $ Map.alter plusPtrStake cred acc
