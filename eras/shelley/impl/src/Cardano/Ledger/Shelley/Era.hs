{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-unsafe-ledger-internal #-}
#endif

module Cardano.Ledger.Shelley.Era (
  ShelleyEra,
  BBODY,
  DELEG,
  DELEGS,
  DELPL,
  EPOCH,
  LEDGER,
  LEDGERS,
  MIR,
  NEWEPOCH,
  NEWPP,
  POOL,
  POOLREAP,
  PPUP,
  RUPD,
  SNAP,
  TICK,
  TICKF,
  UPEC,
  UTXO,
  UTXOW,
  hardforkAllegraAggregatedRewards,
  hardforkAlonzoAllowMIRTransfer,
  hardforkAlonzoValidatePoolAccountAddressNetID,
  hardforkBabbageForgoRewardPrefilter,
  hardforkConwayDisallowDuplicatedVRFKeys,
) where

import Cardano.Ledger.BaseTypes (ProtVer (pvMajor), natVersion)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraRule, EraTxLevel (..), STxTopLevel, Value)
import Cardano.Ledger.Internal.Era (ShelleyEra)

instance EraTxLevel ShelleyEra where
  type STxLevel l ShelleyEra = STxTopLevel l ShelleyEra

type instance Value ShelleyEra = Coin

data BBODY era

data DELEG era

data DELEGS era

data DELPL era

data EPOCH era

data LEDGER era

data LEDGERS era

data MIR era

data NEWEPOCH era

data NEWPP era

data POOL era

data POOLREAP era

data PPUP era

data RUPD era

data SNAP era

data TICK era

data TICKF era

data UPEC era

data UTXO era

data UTXOW era

type instance EraRule "BBODY" ShelleyEra = BBODY ShelleyEra

type instance EraRule "DELEG" ShelleyEra = DELEG ShelleyEra

type instance EraRule "DELEGS" ShelleyEra = DELEGS ShelleyEra

type instance EraRule "DELPL" ShelleyEra = DELPL ShelleyEra

type instance EraRule "EPOCH" ShelleyEra = EPOCH ShelleyEra

type instance EraRule "LEDGER" ShelleyEra = LEDGER ShelleyEra

type instance EraRule "LEDGERS" ShelleyEra = LEDGERS ShelleyEra

type instance EraRule "MIR" ShelleyEra = MIR ShelleyEra

type instance EraRule "NEWEPOCH" ShelleyEra = NEWEPOCH ShelleyEra

type instance EraRule "NEWPP" ShelleyEra = NEWPP ShelleyEra

type instance EraRule "POOL" ShelleyEra = POOL ShelleyEra

type instance EraRule "POOLREAP" ShelleyEra = POOLREAP ShelleyEra

type instance EraRule "PPUP" ShelleyEra = PPUP ShelleyEra

type instance EraRule "RUPD" ShelleyEra = RUPD ShelleyEra

type instance EraRule "SNAP" ShelleyEra = SNAP ShelleyEra

type instance EraRule "TICK" ShelleyEra = TICK ShelleyEra

type instance EraRule "TICKF" ShelleyEra = TICKF ShelleyEra

type instance EraRule "UPEC" ShelleyEra = UPEC ShelleyEra

type instance EraRule "UTXO" ShelleyEra = UTXO ShelleyEra

type instance EraRule "UTXOW" ShelleyEra = UTXOW ShelleyEra

hardforkAllegraAggregatedRewards :: ProtVer -> Bool
hardforkAllegraAggregatedRewards pv = pvMajor pv > natVersion @2

-- | Starting with protocol version 5, the MIR certs will also be
-- able to transfer funds between the reserves and the treasury.
-- Additionally, the semantics for the pervious functionality will
-- change a bit. Before version 5 redundancies in the instantaneous
-- reward mapping were handled by overriding. Now they are handled
-- by adding the values and allowing for negatives updates, provided
-- the sum for each key remains positive.
hardforkAlonzoAllowMIRTransfer ::
  ProtVer ->
  Bool
hardforkAlonzoAllowMIRTransfer pv = pvMajor pv > natVersion @4

-- | Starting with protocol version 5, we will validate the network ID
-- for the account address listed in stake pool registration certificates.
hardforkAlonzoValidatePoolAccountAddressNetID ::
  ProtVer ->
  Bool
hardforkAlonzoValidatePoolAccountAddressNetID pv = pvMajor pv > natVersion @4

-- | Starting with protocol version 7, the reward calculation no longer
-- filters out unregistered stake addresses at the moment the calculation begins.
-- See the Shelley Ledger Errata 17.2.
hardforkBabbageForgoRewardPrefilter :: ProtVer -> Bool
hardforkBabbageForgoRewardPrefilter pv = pvMajor pv > natVersion @6

hardforkConwayDisallowDuplicatedVRFKeys ::
  ProtVer ->
  Bool
hardforkConwayDisallowDuplicatedVRFKeys pv = pvMajor pv > natVersion @10
