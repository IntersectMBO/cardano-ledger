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

  -- * Deprecated
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
) where

import Cardano.Ledger.BaseTypes (ProtVer (pvMajor), natVersion)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (EraRule, EraTxLevel (..), STxTopLevel, Value)
import Cardano.Ledger.Internal.Era (ShelleyEra)

instance EraTxLevel ShelleyEra where
  type STxLevel l ShelleyEra = STxTopLevel l ShelleyEra

type instance Value ShelleyEra = Coin

data BBODY era

type ShelleyBBODY = BBODY

{-# DEPRECATED ShelleyBBODY "In favor of `BBODY`" #-}

data DELEG era

type ShelleyDELEG = DELEG

{-# DEPRECATED ShelleyDELEG "In favor of `DELEG`" #-}

data DELEGS era

type ShelleyDELEGS = DELEGS

{-# DEPRECATED ShelleyDELEGS "In favor of `DELEGS`" #-}

data DELPL era

type ShelleyDELPL = DELPL

{-# DEPRECATED ShelleyDELPL "In favor of `DELPL`" #-}

data EPOCH era

type ShelleyEPOCH = EPOCH

{-# DEPRECATED ShelleyEPOCH "In favor of `EPOCH`" #-}

data LEDGER era

type ShelleyLEDGER = LEDGER

{-# DEPRECATED ShelleyLEDGER "In favor of `LEDGER`" #-}

data LEDGERS era

type ShelleyLEDGERS = LEDGERS

{-# DEPRECATED ShelleyLEDGERS "In favor of `LEDGERS`" #-}

data MIR era

type ShelleyMIR = MIR

{-# DEPRECATED ShelleyMIR "In favor of `MIR`" #-}

data NEWEPOCH era

type ShelleyNEWEPOCH = NEWEPOCH

{-# DEPRECATED ShelleyNEWEPOCH "In favor of `NEWEPOCH`" #-}

data NEWPP era

type ShelleyNEWPP = NEWPP

{-# DEPRECATED ShelleyNEWPP "In favor of `NEWPP`" #-}

data POOL era

type ShelleyPOOL = POOL

{-# DEPRECATED ShelleyPOOL "In favor of `POOL`" #-}

data POOLREAP era

type ShelleyPOOLREAP = POOLREAP

{-# DEPRECATED ShelleyPOOLREAP "In favor of `POOLREAP`" #-}

data PPUP era

type ShelleyPPUP = PPUP

{-# DEPRECATED ShelleyPPUP "In favor of `PPUP`" #-}

data RUPD era

type ShelleyRUPD = RUPD

{-# DEPRECATED ShelleyRUPD "In favor of `RUPD`" #-}

data SNAP era

type ShelleySNAP = SNAP

{-# DEPRECATED ShelleySNAP "In favor of `SNAP`" #-}

data TICK era

type ShelleyTICK = TICK

{-# DEPRECATED ShelleyTICK "In favor of `TICK`" #-}

data TICKF era

type ShelleyTICKF = TICKF

{-# DEPRECATED ShelleyTICKF "In favor of `TICKF`" #-}

data UPEC era

type ShelleyUPEC = UPEC

{-# DEPRECATED ShelleyUPEC "In favor of `UPEC`" #-}

data UTXO era

type ShelleyUTXO = UTXO

{-# DEPRECATED ShelleyUTXO "In favor of `UTXO`" #-}

data UTXOW era

type ShelleyUTXOW = UTXOW

{-# DEPRECATED ShelleyUTXOW "In favor of `UTXOW`" #-}

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
