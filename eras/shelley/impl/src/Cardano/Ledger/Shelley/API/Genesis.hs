{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Shelley.API.Genesis where

import Cardano.Ledger.BaseTypes (BlocksMade (..), EpochNo (EpochNo))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types (
  AccountState (AccountState),
  CertState (CertState),
  Coin (Coin),
  DState (..),
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
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  StashedAVVMAddresses,
  smartUTxOState,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (coinBalance)
import Cardano.Ledger.Val (Val (..))
import Data.Default.Class (Default, def)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Lens.Micro ((&), (.~))

-- | Indicates that this era may be bootstrapped from 'ShelleyGenesis'.
class
  ( EraTxOut era
  , Default (StashedAVVMAddresses era)
  , EraGov era
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

{-# DEPRECATED CanStartFromGenesis "Use `Cardano.Ledger.Shelley.Transition.EraTransition` instead" #-}
{-# DEPRECATED fromShelleyPParams "Use `Cardano.Ledger.Shelley.Transition.tcInitialPParamsG` instead" #-}
{-# DEPRECATED initialState "Use `Cardano.Ledger.Shelley.Transition.createInitialState` instead" #-}

instance
  Crypto c =>
  CanStartFromGenesis (ShelleyEra c)
  where
  fromShelleyPParams _ = id

-- | Helper function for constructing the initial state for any era
initialStateFromGenesis ::
  forall era.
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
        ( LedgerState
            (smartUTxOState (fromShelleyPParams ag pp) initialUtxo (Coin 0) (Coin 0) govSt zero)
            (CertState def def dState)
        )
        emptySnapShots
        def
    )
    SNothing
    (PoolDistr Map.empty mempty)
    def
  where
    initialEpochNo = EpochNo 0
    initialUtxo = genesisUTxO sg
    reserves = word64ToCoin (sgMaxLovelaceSupply sg) <-> coinBalance initialUtxo
    genDelegs = sgGenDelegs sg
    pp = sgProtocolParams sg
    govSt =
      def
        & curPParamsGovStateL .~ fromShelleyPParams ag pp
        & prevPParamsGovStateL .~ fromShelleyPParams ag pp

    dState :: DState era
    dState =
      DState
        { dsUnified = UM.empty
        , dsFutureGenDelegs = Map.empty
        , dsGenDelegs = GenDelegs genDelegs
        , dsIRewards = def
        }
