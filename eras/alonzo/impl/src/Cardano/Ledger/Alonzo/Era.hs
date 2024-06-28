{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Era (
  AlonzoEra,
  AlonzoLedgerState (..),
  AlonzoUTXO,
  AlonzoUTXOS,
  AlonzoUTXOW,
  AlonzoBBODY,
  AlonzoLEDGER,
)
where

import Cardano.Ledger.Binary (DecCBOR (..), DecShareCBOR, EncCBOR, Interns, decNoShareCBOR)
import Cardano.Ledger.Binary.Decoding (DecShareCBOR (..))
import Cardano.Ledger.Binary.Encoding (EncCBOR (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash)
import Cardano.Ledger.Mary (MaryEra, MaryValue)
import Cardano.Ledger.Shelley.API (KeyRole (..))
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  HasLedgerState (..),
  LedgerState,
  lsCertStateL,
  lsUTxOStateL,
 )
import Cardano.Ledger.Shelley.Rules
import Control.DeepSeq (NFData)
import Data.Default.Class (Default)
import GHC.Generics (Generic)
import Lens.Micro (lens)
import Lens.Micro.Type (Lens')
import NoThunks.Class (NoThunks)

-- =====================================================

-- | The Alonzo era
data AlonzoEra c

instance Crypto c => Era (AlonzoEra c) where
  type EraCrypto (AlonzoEra c) = c
  type PreviousEra (AlonzoEra c) = MaryEra c
  type ProtVerLow (AlonzoEra c) = 5
  type ProtVerHigh (AlonzoEra c) = 6

  eraName = "Alonzo"

type instance Value (AlonzoEra c) = MaryValue c

newtype AlonzoLedgerState era = AlonzoLedgerState {unAlonzoLedgerState :: LedgerState era}
  deriving (Generic, Default)

alonzoLedgerStateL :: Lens' (AlonzoLedgerState era) (LedgerState era)
alonzoLedgerStateL = lens unAlonzoLedgerState (\x y -> x {unAlonzoLedgerState = y})

instance (Crypto c, EraGov (AlonzoEra c), EraTxOut (AlonzoEra c)) => HasLedgerState (AlonzoEra c) where
  type EraLedgerState (AlonzoEra c) = AlonzoLedgerState (AlonzoEra c)
  hlsUTxOStateL = alonzoLedgerStateL . lsUTxOStateL
  hlsCertStateL = alonzoLedgerStateL . lsCertStateL

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (AlonzoLedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (AlonzoLedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  ) =>
  NoThunks (AlonzoLedgerState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (AlonzoLedgerState era)

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  EncCBOR (AlonzoLedgerState era)
  where
  encCBOR (AlonzoLedgerState ls) = encCBOR ls

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecCBOR (AlonzoLedgerState era)
  where
  decCBOR = decNoShareCBOR

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecShareCBOR (AlonzoLedgerState era)
  where
  type
    Share (AlonzoLedgerState era) =
      ( Interns (Credential 'Staking (EraCrypto era))
      , Interns (KeyHash 'StakePool (EraCrypto era))
      )
  decSharePlusCBOR = AlonzoLedgerState <$> decSharePlusCBOR

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- These rules are new or changed in Alonzo

data AlonzoUTXOS era

type instance EraRule "UTXOS" (AlonzoEra c) = AlonzoUTXOS (AlonzoEra c)

data AlonzoUTXO era

type instance EraRule "UTXO" (AlonzoEra c) = AlonzoUTXO (AlonzoEra c)

data AlonzoUTXOW era

type instance EraRule "UTXOW" (AlonzoEra c) = AlonzoUTXOW (AlonzoEra c)

data AlonzoLEDGER era

type instance EraRule "LEDGER" (AlonzoEra c) = AlonzoLEDGER (AlonzoEra c)

data AlonzoBBODY era

type instance EraRule "BBODY" (AlonzoEra c) = AlonzoBBODY (AlonzoEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (AlonzoEra c) = ShelleyDELEG (AlonzoEra c)

type instance EraRule "DELEGS" (AlonzoEra c) = ShelleyDELEGS (AlonzoEra c)

type instance EraRule "DELPL" (AlonzoEra c) = ShelleyDELPL (AlonzoEra c)

type instance EraRule "EPOCH" (AlonzoEra c) = ShelleyEPOCH (AlonzoEra c)

type instance EraRule "LEDGERS" (AlonzoEra c) = ShelleyLEDGERS (AlonzoEra c)

type instance EraRule "MIR" (AlonzoEra c) = ShelleyMIR (AlonzoEra c)

type instance EraRule "NEWEPOCH" (AlonzoEra c) = ShelleyNEWEPOCH (AlonzoEra c)

type instance EraRule "NEWPP" (AlonzoEra c) = ShelleyNEWPP (AlonzoEra c)

type instance EraRule "POOL" (AlonzoEra c) = ShelleyPOOL (AlonzoEra c)

type instance EraRule "POOLREAP" (AlonzoEra c) = ShelleyPOOLREAP (AlonzoEra c)

type instance EraRule "PPUP" (AlonzoEra c) = ShelleyPPUP (AlonzoEra c)

type instance EraRule "RUPD" (AlonzoEra c) = ShelleyRUPD (AlonzoEra c)

type instance EraRule "SNAP" (AlonzoEra c) = ShelleySNAP (AlonzoEra c)

type instance EraRule "TICK" (AlonzoEra c) = ShelleyTICK (AlonzoEra c)

type instance EraRule "TICKF" (AlonzoEra c) = ShelleyTICKF (AlonzoEra c)

type instance EraRule "UPEC" (AlonzoEra c) = ShelleyUPEC (AlonzoEra c)
