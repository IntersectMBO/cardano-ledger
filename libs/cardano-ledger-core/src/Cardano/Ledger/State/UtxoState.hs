{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.State.UtxoState (
  UTxOState,
  UtxoState (
    ..,
    UTxOState,
    utxosUtxo,
    utxosDeposited,
    utxosFees,
    utxosGovState,
    utxosInstantStake,
    utxosDonation
  ),
  mkUtxoState,
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

data UtxoState era = UtxoState
  { usUTxO :: !(UTxO era)
  , usDeposited :: !Coin
  -- ^ Total amount of deposits in the system
  , usFees :: !Coin
  -- ^ Amount of fees accrued this epoch
  , usGovState :: !(GovState era)
  , usInstantStake :: !(InstantStake era)
  -- ^ There is an invariant in this `InstantStake` that it must match what is in the `UTxO` that is
  -- in this type. In the ledger rules it is updated incrementally, but in various testing scenarios
  -- it is possible to construct this type with the smart constructor `UtxoState`
  , usDonations :: !Coin
  -- ^ Amount of donations accrued this epoch
  }
  deriving (Generic)

type UTxOState era = UtxoState era

pattern UTxOState ::
  UTxO era ->
  Coin ->
  Coin ->
  GovState era ->
  InstantStake era ->
  Coin ->
  UTxOState era
pattern UTxOState {utxosUtxo, utxosDeposited, utxosFees, utxosGovState, utxosInstantStake, utxosDonation} <-
  UtxoState
    { usUTxO = utxosUtxo
    , usDeposited = utxosDeposited
    , usFees = utxosFees
    , usGovState = utxosGovState
    , usInstantStake = utxosInstantStake
    , usDonations = utxosDonation
    }
  where
    UTxOState utxo deposits fees govState instantStake donations =
      UtxoState utxo deposits fees govState instantStake donations
{-# COMPLETE UTxOState #-}

-- | Construct `UtxoState` in a way that invariant for `InstantStake` is not violated.
--
-- /Warning/ - It is important to note that it is an expensive operation to construct `UtxoState`
-- using this function, therefore it must not be used in any of the rules.
mkUtxoState ::
  EraStake era =>
  -- | All of the unspent outputs
  UTxO era ->
  -- | Total amount of deposits in the system
  Coin ->
  -- | Amount of fees accrued this epoch
  Coin ->
  -- | Governance state
  GovState era ->
  -- | Amount of donations accrued this epoch
  Coin ->
  UtxoState era
mkUtxoState utxo deposits fees govState =
  UtxoState utxo deposits fees govState (addInstantStake utxo mempty)

instance
  ( EraTxOut era
  , NFData (GovState era)
  , NFData (InstantStake era)
  ) =>
  NFData (UtxoState era)

deriving instance
  ( EraTxOut era
  , Show (GovState era)
  , Show (InstantStake era)
  ) =>
  Show (UtxoState era)

deriving instance
  ( EraTxOut era
  , Eq (GovState era)
  , Eq (InstantStake era)
  ) =>
  Eq (UtxoState era)

instance
  ( NoThunks (UTxO era)
  , NoThunks (GovState era)
  , NoThunks (InstantStake era)
  ) =>
  NoThunks (UtxoState era)

instance (EraGov era, EraStake era) => Default (UtxoState era) where
  def = UtxoState mempty mempty mempty def mempty mempty

instance CanGetUTxO UtxoState
instance CanSetUTxO UtxoState where
  utxoL = lens usUTxO $ \s u -> s {usUTxO = u}
  {-# INLINE utxoL #-}

instance CanGetInstantStake UtxoState
instance CanSetInstantStake UtxoState where
  instantStakeL = lens usInstantStake $ \s is -> s {usInstantStake = is}
  {-# INLINE instantStakeL #-}

instance
  ( EraTxOut era
  , EraStake era
  , EncCBOR (GovState era)
  ) =>
  EncCBOR (UtxoState era)
  where
  encCBOR utxos@(UtxoState _ _ _ _ _ _) =
    let UtxoState {..} = utxos
     in encode $
          Rec UtxoState
            -- We need to define encoder with MemPack manually here instead of changing the `EncCBOR`
            -- instance for `UTxO` in order to not affect some of the ledger state queries.
            !> E (encodeMap encodeMemPack encodeMemPack . unUTxO) usUTxO
            !> To usDeposited
            !> To usFees
            !> To usGovState
            !> To usInstantStake
            !> To usDonations

instance (EraTxOut era, EraGov era, EraStake era) => DecShareCBOR (UtxoState era) where
  type
    Share (UtxoState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decShareCBOR is@(cs, _, _, _) =
    decodeRecordNamed "UtxoState" (const 6) $ do
      usUTxO <- decShareCBOR cs
      usDeposited <- decCBOR
      usFees <- decCBOR
      usGovState <- decShareCBOR is
      usInstantStake <- decShareCBOR cs
      usDonations <- decCBOR
      pure UtxoState {..}

instance (EraTxOut era, EraGov era, EraStake era) => ToCBOR (UtxoState era) where
  toCBOR = toEraCBOR @era

instance (EraTxOut era, EraGov era, EraStake era) => FromCBOR (UtxoState era) where
  fromCBOR = fromEraShareCBOR @era

instance (EraTxOut era, EraGov era, EraStake era) => ToJSON (UtxoState era) where
  toJSON = object . toUtxoStatePairs
  toEncoding = pairs . mconcat . toUtxoStatePairs

toUtxoStatePairs ::
  (EraTxOut era, EraGov era, EraStake era, KeyValue e a) => UtxoState era -> [a]
toUtxoStatePairs utxoState@(UtxoState _ _ _ _ _ _) =
  let UtxoState {..} = utxoState
   in [ "utxo" .= usUTxO
      , "deposited" .= usDeposited
      , "fees" .= usFees
      , "ppups" .= usGovState
      , "stake" .= usInstantStake
      , "donations" .= usDonations
      ]
