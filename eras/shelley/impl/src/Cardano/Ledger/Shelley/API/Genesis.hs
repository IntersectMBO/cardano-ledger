{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.API.Genesis where

import Cardano.Ledger.BaseTypes (BlocksMade (..))
import Cardano.Ledger.Core (EraRule, EraTxOut, PParams)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Types
  ( AccountState (AccountState),
    Coin (Coin),
    DPState (DPState),
    DState (_genDelegs),
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
import Cardano.Ledger.Shelley.EpochBoundary (emptySnapShots)
import Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses, smartUTxOState)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Cardano.Ledger.Shelley.UTxO (coinBalance)
import Cardano.Ledger.Val (Val ((<->)))
import Control.State.Transition (STS (State))
import Data.Default.Class (Default, def)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map

-- | Indicates that this era may be bootstrapped from 'ShelleyGenesis'.
class CanStartFromGenesis era where
  -- | Additional genesis configuration necessary for this era.
  type AdditionalGenesisConfig era :: Type

  type AdditionalGenesisConfig era = ()

  -- | Construct an initial state given a 'ShelleyGenesis' and any appropriate
  -- 'AdditionalGenesisConfig' for the era.
  initialState ::
    ShelleyGenesis era ->
    AdditionalGenesisConfig era ->
    NewEpochState era

instance
  ( Crypto c,
    Default (State (EraRule "PPUP" (ShelleyEra c)))
  ) =>
  CanStartFromGenesis (ShelleyEra c)
  where
  initialState = initialStateFromGenesis const

-- | Helper function for constructing the initial state for any era
initialStateFromGenesis ::
  ( EraTxOut era,
    Default (State (EraRule "PPUP" era)),
    Default (StashedAVVMAddresses era)
  ) =>
  -- | Function to extend ShelleyPParams into PParams for the specific era
  (ShelleyPParams era -> g -> PParams era) ->
  ShelleyGenesis era ->
  -- | Genesis type
  g ->
  NewEpochState era
initialStateFromGenesis extendPPWithGenesis' sg ag =
  NewEpochState
    initialEpochNo
    (BlocksMade Map.empty)
    (BlocksMade Map.empty)
    ( EpochState
        (AccountState (Coin 0) reserves)
        emptySnapShots
        ( LedgerState
            (smartUTxOState initialUtxo (Coin 0) (Coin 0) def)
            (DPState (def {_genDelegs = GenDelegs genDelegs}) def)
        )
        (extendPPWithGenesis' pp ag)
        (extendPPWithGenesis' pp ag)
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
