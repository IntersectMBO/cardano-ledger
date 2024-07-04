{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Babel.LedgerState.Types where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (EraCrypto, EraGov, EraTxOut, GovState)
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.FRxO (FRxO)
import Cardano.Ledger.Shelley.API (LedgerState (..), UTxOState (..))
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  HasLedgerState (..),
  IncrementalStake,
 )
import Cardano.Ledger.UTxO (UTxO)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default (def))
import GHC.Generics (Generic)
import Lens.Micro (lens, (&), (.~), (^.))
import Lens.Micro.Type (Lens')

-- type instance Ledger (ConwayEra c) = LedgerStateTemp (ConwayEra c)

-- | The state associated with a 'Ledger'.
data LedgerStateTemp era = LedgerStateTemp
  { lstUTxOState :: !(UTxOStateTemp era)
  -- ^ The current unspent transaction outputs.
  , lstCertState :: !(CertState era)
  }
  deriving (Generic)

instance Default (UTxOStateTemp era) => Default (LedgerStateTemp era) where
  def = LedgerStateTemp def def

instance Era era => HasLedgerState LedgerStateTemp era where
  hlsUtxoStateL = lens getter setter
    where
      getter s = toUTxOState (s ^. lstUtxoStateL)
      setter s b = s & lstUtxoStateL .~ fromUTxOState b
  hlsCertStateL = lstCertStateL
  mkLedgerState utxos = LedgerStateTemp (fromUTxOState utxos)

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (LedgerStateTemp era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (LedgerStateTemp era)

-- Conversion

-- | Convert from LedgerState to LedgerStateTemp
fromLedgerState :: Era era => LedgerState era -> LedgerStateTemp era
fromLedgerState LedgerState {lsUTxOState, lsCertState} =
  LedgerStateTemp
    { lstUTxOState = fromUTxOState lsUTxOState
    , lstCertState = lsCertState
    }

-- | Convert from UTxOState to UTxOStateTemp
fromUTxOState :: Era era => UTxOState era -> UTxOStateTemp era
fromUTxOState
  UTxOState
    { utxosUtxo
    , utxosDeposited
    , utxosFees
    , utxosGovState
    , utxosStakeDistr
    , utxosDonation
    } =
    UTxOStateTemp
      { utxostUtxo = utxosUtxo
      , utxostFrxo = mempty
      , utxostDeposited = utxosDeposited
      , utxostFees = utxosFees
      , utxostGovState = utxosGovState
      , utxostStakeDistr = utxosStakeDistr
      , utxostDonation = utxosDonation
      }

-- | Convert from LedgerStateTemp to LedgerState
toLedgerState :: LedgerStateTemp era -> LedgerState era
toLedgerState LedgerStateTemp {lstUTxOState, lstCertState} =
  LedgerState
    { lsUTxOState = toUTxOState lstUTxOState
    , lsCertState = lstCertState
    }

-- | Convert from UTxOStateTemp to UTxOState
toUTxOState :: UTxOStateTemp era -> UTxOState era
toUTxOState
  UTxOStateTemp
    { utxostUtxo
    , utxostDeposited
    , utxostFees
    , utxostGovState
    , utxostStakeDistr
    , utxostDonation
    } =
    UTxOState
      { utxosUtxo = utxostUtxo
      , utxosDeposited = utxostDeposited
      , utxosFees = utxostFees
      , utxosGovState = utxostGovState
      , utxosStakeDistr = utxostStakeDistr
      , utxosDonation = utxostDonation
      }

--------

data UTxOStateTemp era = UTxOStateTemp
  { utxostUtxo :: !(UTxO era)
  , utxostFrxo :: !(FRxO era)
  , utxostDeposited :: Coin
  -- ^ This field is left lazy, because we only use it for assertions
  , utxostFees :: !Coin
  , utxostGovState :: !(GovState era)
  , utxostStakeDistr :: !(IncrementalStake (EraCrypto era))
  , utxostDonation :: !Coin
  }
  deriving (Generic)

-- Lenses

lstUtxoStateL :: Lens' (LedgerStateTemp era) (UTxOStateTemp era)
lstUtxoStateL = lens lstUTxOState (\s x -> s {lstUTxOState = x})

lstCertStateL :: Lens' (LedgerStateTemp era) (CertState era)
lstCertStateL = lens lstCertState (\s x -> s {lstCertState = x})

utxostUtxoL :: Lens' (UTxOStateTemp era) (UTxO era)
utxostUtxoL = lens utxostUtxo (\s x -> s {utxostUtxo = x})

utxostFrxoL :: Lens' (UTxOStateTemp era) (FRxO era)
utxostFrxoL = lens utxostFrxo (\s x -> s {utxostFrxo = x})

utxostDepositedL :: Lens' (UTxOStateTemp era) Coin
utxostDepositedL = lens utxostDeposited (\s x -> s {utxostDeposited = x})

utxostFeesL :: Lens' (UTxOStateTemp era) Coin
utxostFeesL = lens utxostFees (\s x -> s {utxostFees = x})

utxostGovStateL :: Lens' (UTxOStateTemp era) (GovState era)
utxostGovStateL = lens utxostGovState (\s x -> s {utxostGovState = x})

utxostStakeDistrL :: Lens' (UTxOStateTemp era) (IncrementalStake (EraCrypto era))
utxostStakeDistrL = lens utxostStakeDistr (\s x -> s {utxostStakeDistr = x})

utxostDonationL :: Lens' (UTxOStateTemp era) Coin
utxostDonationL = lens utxostDonation (\s x -> s {utxostDonation = x})

-- ====================================================

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

instance EraGov era => Default (UTxOStateTemp era) where
  def = UTxOStateTemp mempty mempty mempty mempty def mempty mempty

-------

instance
  ( EraTxOut era
  , NFData (GovState era)
  ) =>
  NFData (UTxOStateTemp era)

deriving stock instance
  ( EraTxOut era
  , Show (GovState era)
  ) =>
  Show (UTxOStateTemp era)

deriving stock instance
  ( EraTxOut era
  , Eq (GovState era)
  ) =>
  Eq (UTxOStateTemp era)
