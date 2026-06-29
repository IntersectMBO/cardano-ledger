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
  ShelleyBBODY,
  ShelleyDELEG,
  ShelleyDELEGS,
  ShelleyDELPL,
  ShelleyEPOCH,
  ShelleyLEDGER,
  ShelleyLEDGERS,
  ShelleyMIR,
  ShelleyNEWEPOCH,
  ShelleyNEWPP,
  ShelleyPOOL,
  ShelleyPOOLREAP,
  ShelleyPPUP,
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
  ShelleyTICKF,
  ShelleyUPEC,
  ShelleyUTXO,
  ShelleyUTXOW,
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

data ShelleyBBODY era

data ShelleyDELEG era

data ShelleyDELEGS era

data ShelleyDELPL era

data ShelleyEPOCH era

data ShelleyLEDGER era

data ShelleyLEDGERS era

data ShelleyMIR era

data ShelleyNEWEPOCH era

data ShelleyNEWPP era

data ShelleyPOOL era

data ShelleyPOOLREAP era

data ShelleyPPUP era

data ShelleyRUPD era

data ShelleySNAP era

data ShelleyTICK era

data ShelleyTICKF era

data ShelleyUPEC era

data ShelleyUTXO era

data ShelleyUTXOW era

type instance EraRule "BBODY" ShelleyEra = ShelleyBBODY ShelleyEra

type instance EraRule "DELEG" ShelleyEra = ShelleyDELEG ShelleyEra

type instance EraRule "DELEGS" ShelleyEra = ShelleyDELEGS ShelleyEra

type instance EraRule "DELPL" ShelleyEra = ShelleyDELPL ShelleyEra

type instance EraRule "EPOCH" ShelleyEra = ShelleyEPOCH ShelleyEra

type instance EraRule "LEDGER" ShelleyEra = ShelleyLEDGER ShelleyEra

type instance EraRule "LEDGERS" ShelleyEra = ShelleyLEDGERS ShelleyEra

type instance EraRule "MIR" ShelleyEra = ShelleyMIR ShelleyEra

type instance EraRule "NEWEPOCH" ShelleyEra = ShelleyNEWEPOCH ShelleyEra

type instance EraRule "NEWPP" ShelleyEra = ShelleyNEWPP ShelleyEra

type instance EraRule "POOL" ShelleyEra = ShelleyPOOL ShelleyEra

type instance EraRule "POOLREAP" ShelleyEra = ShelleyPOOLREAP ShelleyEra

type instance EraRule "PPUP" ShelleyEra = ShelleyPPUP ShelleyEra

type instance EraRule "RUPD" ShelleyEra = ShelleyRUPD ShelleyEra

type instance EraRule "SNAP" ShelleyEra = ShelleySNAP ShelleyEra

type instance EraRule "TICK" ShelleyEra = ShelleyTICK ShelleyEra

type instance EraRule "TICKF" ShelleyEra = ShelleyTICKF ShelleyEra

type instance EraRule "UPEC" ShelleyEra = ShelleyUPEC ShelleyEra

type instance EraRule "UTXO" ShelleyEra = ShelleyUTXO ShelleyEra

type instance EraRule "UTXOW" ShelleyEra = ShelleyUTXOW ShelleyEra

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
