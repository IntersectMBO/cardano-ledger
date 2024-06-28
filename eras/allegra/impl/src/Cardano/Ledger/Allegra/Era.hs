{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Allegra.Era (
  AllegraEra,
  AllegraUTXO,
  AllegraUTXOW,
  AllegraLedgerState (..),
) where

import Cardano.Ledger.Binary (DecCBOR, DecShareCBOR (decSharePlusCBOR), EncCBOR, Interns)
import Cardano.Ledger.Binary.Decoding (DecCBOR (..), Share, decNoShareCBOR)
import Cardano.Ledger.Binary.Encoding (encCBOR)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  HasLedgerState (..),
  LedgerState (..),
  lsCertStateL,
  lsUTxOStateL,
 )
import Cardano.Ledger.Shelley.Rules
import Control.DeepSeq (NFData)
import Data.Default.Class (Default)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

-- | The Allegra era
data AllegraEra c

instance Crypto c => Era (AllegraEra c) where
  type PreviousEra (AllegraEra c) = ShelleyEra c
  type EraCrypto (AllegraEra c) = c
  type ProtVerLow (AllegraEra c) = 3

  eraName = "Allegra"

newtype AllegraLedgerState era = AllegraLedgerState {unAllegraLedgerState :: LedgerState era}
  deriving (Generic, Default)

allegraLedgerStateL :: Lens' (AllegraLedgerState era) (LedgerState era)
allegraLedgerStateL = lens unAllegraLedgerState (\x y -> x {unAllegraLedgerState = y})

instance (Crypto c, EraGov (AllegraEra c), EraTxOut (AllegraEra c)) => HasLedgerState (AllegraEra c) where
  type EraLedgerState (AllegraEra c) = AllegraLedgerState (AllegraEra c)
  hlsUTxOStateL = allegraLedgerStateL . lsUTxOStateL
  hlsCertStateL = allegraLedgerStateL . lsCertStateL

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (AllegraLedgerState era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (AllegraLedgerState era)

instance
  ( EraTxOut era
  , NoThunks (GovState era)
  ) =>
  NoThunks (AllegraLedgerState era)

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (AllegraLedgerState era)

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  EncCBOR (AllegraLedgerState era)
  where
  encCBOR (AllegraLedgerState ls) = encCBOR ls

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecCBOR (AllegraLedgerState era)
  where
  decCBOR = decNoShareCBOR

instance
  ( EraTxOut era
  , EraGov era
  ) =>
  DecShareCBOR (AllegraLedgerState era)
  where
  type
    Share (AllegraLedgerState era) =
      ( Interns (Credential 'Staking (EraCrypto era))
      , Interns (KeyHash 'StakePool (EraCrypto era))
      )
  decSharePlusCBOR = AllegraLedgerState <$> decSharePlusCBOR

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

-- | No context is needed to translate from Shelley to Allegra.
type instance TranslationContext (AllegraEra c) = ()

type instance Value (AllegraEra _) = Coin

-- These rules are all inherited from Shelley

type instance EraRule "BBODY" (AllegraEra c) = ShelleyBBODY (AllegraEra c)

type instance EraRule "DELEG" (AllegraEra c) = ShelleyDELEG (AllegraEra c)

type instance EraRule "DELEGS" (AllegraEra c) = ShelleyDELEGS (AllegraEra c)

type instance EraRule "DELPL" (AllegraEra c) = ShelleyDELPL (AllegraEra c)

type instance EraRule "EPOCH" (AllegraEra c) = ShelleyEPOCH (AllegraEra c)

type instance EraRule "LEDGER" (AllegraEra c) = ShelleyLEDGER (AllegraEra c)

type instance EraRule "LEDGERS" (AllegraEra c) = ShelleyLEDGERS (AllegraEra c)

type instance EraRule "MIR" (AllegraEra c) = ShelleyMIR (AllegraEra c)

type instance EraRule "NEWEPOCH" (AllegraEra c) = ShelleyNEWEPOCH (AllegraEra c)

type instance EraRule "NEWPP" (AllegraEra c) = ShelleyNEWPP (AllegraEra c)

type instance EraRule "POOL" (AllegraEra c) = ShelleyPOOL (AllegraEra c)

type instance EraRule "POOLREAP" (AllegraEra c) = ShelleyPOOLREAP (AllegraEra c)

type instance EraRule "PPUP" (AllegraEra c) = ShelleyPPUP (AllegraEra c)

type instance EraRule "RUPD" (AllegraEra c) = ShelleyRUPD (AllegraEra c)

type instance EraRule "SNAP" (AllegraEra c) = ShelleySNAP (AllegraEra c)

type instance EraRule "TICK" (AllegraEra c) = ShelleyTICK (AllegraEra c)

type instance EraRule "TICKF" (AllegraEra c) = ShelleyTICKF (AllegraEra c)

type instance EraRule "UPEC" (AllegraEra c) = ShelleyUPEC (AllegraEra c)

-- These rules are defined anew in the Allegra era

data AllegraUTXO era

type instance EraRule "UTXO" (AllegraEra c) = AllegraUTXO (AllegraEra c)

data AllegraUTXOW era

type instance EraRule "UTXOW" (AllegraEra c) = AllegraUTXOW (AllegraEra c)
