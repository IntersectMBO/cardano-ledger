{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.State.UtxoState (
  UTxOState (..),
) where

import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State.Governance
import Cardano.Ledger.State.Stake
import Cardano.Ledger.State.UTxO
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks)

data UTxOState era = UTxOState
  { utxosUtxo :: !(UTxO era)
  , utxosDeposited :: !Coin
  , utxosFees :: !Coin
  , utxosGovState :: !(GovState era)
  , utxosInstantStake :: !(InstantStake era)
  -- ^ There is an invariant in this `InstantStake` that it must match what is in the `UTxO` that is
  -- in this type. In the ledger rules it is updated incrementally, but in various testing scenarios
  -- it is possible to construct this type with the smart constructor `mkUtxoState`
  , utxosDonation :: !Coin
  }
  deriving (Generic)

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (InstantStake era)
  ) =>
  NFData (UTxOState era)

deriving instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (InstantStake era)
  ) =>
  Show (UTxOState era)

deriving instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (InstantStake era)
  ) =>
  Eq (UTxOState era)

instance
  ( NoThunks (UTxO era)
  , NoThunks (GovState era)
  , NoThunks (InstantStake era)
  ) =>
  NoThunks (UTxOState era)

instance (EraGov era, EraStake era) => Default (UTxOState era) where
  def = UTxOState mempty mempty mempty def mempty mempty

instance CanGetUTxO UTxOState
instance CanSetUTxO UTxOState where
  utxoL = lens utxosUtxo $ \s u -> s {utxosUtxo = u}
  {-# INLINE utxoL #-}

instance CanGetInstantStake UTxOState
instance CanSetInstantStake UTxOState where
  instantStakeL = lens utxosInstantStake $ \s is -> s {utxosInstantStake = is}
  {-# INLINE instantStakeL #-}

instance
  ( EraTxOut era
  , EraStake era
  , EncCBOR (GovState era)
  ) =>
  EncCBOR (UTxOState era)
  where
  encCBOR utxos@(UTxOState _ _ _ _ _ _) =
    let UTxOState {..} = utxos
     in encode $
          Rec UTxOState
            -- We need to define encoder with MemPack manually here instead of changing the `EncCBOR`
            -- instance for `UTxO` in order to not affect some of the ledger state queries.
            !> E (encodeMap encodeMemPack encodeMemPack . unUTxO) utxosUtxo
            !> To utxosDeposited
            !> To utxosFees
            !> To utxosGovState
            !> To utxosInstantStake
            !> To utxosDonation

instance (EraTxOut era, EraGov era, EraStake era) => DecShareCBOR (UTxOState era) where
  type
    Share (UTxOState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decShareCBOR is@(cs, _, _, _) =
    decodeRecordNamed "UTxOState" (const 6) $ do
      utxosUtxo <- decShareCBOR cs
      utxosDeposited <- decCBOR
      utxosFees <- decCBOR
      utxosGovState <- decShareCBOR is
      utxosInstantStake <- decShareCBOR cs
      utxosDonation <- decCBOR
      pure UTxOState {..}

instance (EraTxOut era, EraGov era, EraStake era) => ToCBOR (UTxOState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraStake era) => FromCBOR (UTxOState era) where
  fromCBOR = fromEraShareCBOR @era

instance (EraTxOut era, EraGov era, EraStake era) => ToJSON (UTxOState era) where
  toJSON = object . toUTxOStatePairs
  toEncoding = pairs . mconcat . toUTxOStatePairs

toUTxOStatePairs ::
  (EraTxOut era, EraGov era, EraStake era, KeyValue e a) => UTxOState era -> [a]
toUTxOStatePairs utxoState@(UTxOState _ _ _ _ _ _) =
  let UTxOState {..} = utxoState
   in [ "utxo" .= utxosUtxo
      , "deposited" .= utxosDeposited
      , "fees" .= utxosFees
      , "ppups" .= utxosGovState
      , "stake" .= utxosInstantStake
      ]
