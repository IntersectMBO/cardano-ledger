{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.API.Genesis where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Core (Era (..), EraRule, EraTxOut, PParams)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types (
  AccountState (AccountState),
  Coin (Coin),
  DPState (DPState),
  DState (dsGenDelegs),
  EpochState (EpochState),
  GenDelegs (GenDelegs),
  LedgerState (LedgerState),
  NewEpochState (NewEpochState),
  PoolDistr (PoolDistr),
  ShelleyGenesis (sgGenDelegs, sgMaxLovelaceSupply, sgProtocolParams),
  StrictMaybe (SNothing),
  genesisUTxO,
  word64ToCoin,
 )
import Cardano.Ledger.Shelley.Core (EraTallyState (..))
import Cardano.Ledger.Shelley.LedgerState (
  PPUPState,
  StashedAVVMAddresses,
  smartUTxOState,
 )
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.UTxO (coinBalance)
import Cardano.Ledger.Val (Val ((<->)))
import Control.State.Transition (STS (..))
import Data.Default.Class (Default, def)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map

-- | Indicates that this era may be bootstrapped from 'ShelleyGenesis'.
class
  ( EraTxOut era
  , Default (StashedAVVMAddresses era)
  , Default (PPUPState era)
  , EraTallyState era
  ) =>
  CanStartFromGenesis era
  where
  -- | Additional genesis configuration necessary for this era.
  type AdditionalGenesisConfig era :: Type

  type AdditionalGenesisConfig era = ()

  -- | Upgrade `PParams` from `ShelleyEra` all the way to the current one.
  fromShelleyPParams ::
    AdditionalGenesisConfig era ->
    PParams (ShelleyEra (EraCrypto era)) ->
    PParams era

  -- | Construct an initial state given a 'ShelleyGenesis' and any appropriate
  -- 'AdditionalGenesisConfig' for the era.
  initialState ::
    ShelleyGenesis (EraCrypto era) ->
    AdditionalGenesisConfig era ->
    NewEpochState era
  initialState = initialStateFromGenesis

instance
  ( Crypto c
  , Default (State (EraRule "PPUP" (ShelleyEra c)))
  ) =>
  CanStartFromGenesis (ShelleyEra c)
  where
  fromShelleyPParams _ = id

-- | Helper function for constructing the initial state for any era
initialStateFromGenesis ::
  CanStartFromGenesis era =>
  -- | Genesis type
  ShelleyGenesis (EraCrypto era) ->
  AdditionalGenesisConfig era ->
  NewEpochState era
initialStateFromGenesis sg ag =
  NewEpochState
    initialEpochNo
    (BlocksMade Map.empty)
    (BlocksMade Map.empty)
    ( EpochState
        (AccountState (Coin 0) reserves)
        emptySnapShots
        ( LedgerState
            (smartUTxOState initialUtxo (Coin 0) (Coin 0) def)
            (DPState (def {dsGenDelegs = GenDelegs genDelegs}) def)
            emptyTallyState
        )
        (fromShelleyPParams ag pp)
        (fromShelleyPParams ag pp)
        def
    )
    SNothing
    (PoolDistr Map.empty)
    def
  where
    initialEpochNo = 0
    initialUtxo = genesisUTxO sg
    reserves = word64ToCoin (sgMaxLovelaceSupply sg) <-> coinBalance initialUtxo
    genDelegs = sgGenDelegs sg
    pp = sgProtocolParams sg
