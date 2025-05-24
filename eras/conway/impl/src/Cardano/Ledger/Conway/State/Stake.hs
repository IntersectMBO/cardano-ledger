{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.State.Stake (
  ConwayInstantStake (..),
  conwayInstantStakeCredentialsL,
  addConwayInstantStake,
  deleteConwayInstantStake,
  resolveConwayInstantStake,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  TokenType (..),
  peekTokenType,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.State.Account ()
import Cardano.Ledger.Conway.TxOut ()
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.State
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Coerce
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

newtype ConwayInstantStake era = ConwayInstantStake
  { cisCredentialStake :: Map.Map (Credential 'Staking) (CompactForm Coin)
  }
  deriving (Generic, Show, Eq, Ord, EncCBOR, NFData, NoThunks, Default, Monoid)

instance DecShareCBOR (ConwayInstantStake era) where
  type Share (ConwayInstantStake era) = Interns (Credential 'Staking)
  decShareCBOR credInterns = do
    peekTokenType >>= \case
      TypeListLen -> toConwayInstantStake <$> decShareCBOR credInterns
      TypeListLen64 -> toConwayInstantStake <$> decShareCBOR credInterns
      TypeListLenIndef -> toConwayInstantStake <$> decShareCBOR credInterns
      _ -> ConwayInstantStake <$> decShareCBOR (credInterns, mempty)
    where
      toConwayInstantStake :: ShelleyInstantStake era -> ConwayInstantStake era
      toConwayInstantStake = ConwayInstantStake . sisCredentialStake

instance Semigroup (ConwayInstantStake era) where
  ConwayInstantStake cs1 <> ConwayInstantStake cs2 =
    ConwayInstantStake (Map.unionWith (<>) cs1 cs2)

instance ToJSON (ConwayInstantStake era) where
  toJSON = object . toIncrementalStakePairs
  toEncoding = pairs . mconcat . toIncrementalStakePairs

toIncrementalStakePairs :: KeyValue e a => ConwayInstantStake era -> [a]
toIncrementalStakePairs iStake@(ConwayInstantStake _) =
  let ConwayInstantStake {..} = iStake -- guard against addition or removal of fields
   in [ "credentials" .= cisCredentialStake
      ]

instance EraStake ConwayEra where
  type InstantStake ConwayEra = ConwayInstantStake ConwayEra
  instantStakeCredentialsL = conwayInstantStakeCredentialsL
  addInstantStake = addConwayInstantStake
  deleteInstantStake = deleteConwayInstantStake
  resolveInstantStake = resolveConwayInstantStake

conwayInstantStakeCredentialsL ::
  Lens' (ConwayInstantStake era) (Map.Map (Credential 'Staking) (CompactForm Coin))
conwayInstantStakeCredentialsL = lens cisCredentialStake $ \is m -> is {cisCredentialStake = m}

addConwayInstantStake ::
  EraTxOut era => UTxO era -> ConwayInstantStake era -> ConwayInstantStake era
addConwayInstantStake = applyUTxOConwayInstantStake (coerce ((+) @Word64))

deleteConwayInstantStake ::
  EraTxOut era => UTxO era -> ConwayInstantStake era -> ConwayInstantStake era
deleteConwayInstantStake = applyUTxOConwayInstantStake (coerce ((-) @Word64))

applyUTxOConwayInstantStake ::
  EraTxOut era =>
  (CompactForm Coin -> CompactForm Coin -> CompactForm Coin) ->
  UTxO era ->
  ConwayInstantStake era ->
  ConwayInstantStake era
applyUTxOConwayInstantStake f (UTxO u) instantInstantStake =
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
    accum ans@(ConwayInstantStake {cisCredentialStake}) out =
      let cc = out ^. compactCoinTxOutL
       in case out ^. addrTxOutL of
            Addr _ _ (StakeRefBase stakingKeyHash) ->
              ConwayInstantStake
                { cisCredentialStake = Map.alter (keepOrDeleteCompact cc) stakingKeyHash cisCredentialStake
                }
            _other -> ans

-- The invariant in `InstantStake` is that stake is never zero.
resolveConwayInstantStake ::
  (EraStake era, InstantStake era ~ ConwayInstantStake era) =>
  ConwayInstantStake era ->
  Accounts era ->
  Stake
resolveConwayInstantStake instantStake accounts =
  Stake $ VMap.fromMap $ resolveActiveInstantStakeCredentials instantStake accounts
{-# INLINE resolveConwayInstantStake #-}
