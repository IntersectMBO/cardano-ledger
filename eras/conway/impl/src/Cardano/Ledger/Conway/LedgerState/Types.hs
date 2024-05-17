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

module Cardano.Ledger.Conway.LedgerState.Types where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (EraCrypto, EraGov, EraTxOut, GovState)
import Cardano.Ledger.FRxO (FRxO)
import Cardano.Ledger.Shelley.LedgerState (
  CertState,
  IncrementalStake,
 )
import Cardano.Ledger.UTxO (UTxO)
import Control.DeepSeq (NFData)
import Data.Default.Class (Default (def))
import GHC.Generics (Generic)

-- type instance Ledger (ConwayEra c) = LedgerStateTemp (ConwayEra c)

-- | The state associated with a 'Ledger'.
data LedgerStateTemp era = LedgerStateTemp
  { lstUTxOState :: !(UTxOStateTemp era)
  -- ^ The current unspent transaction outputs.
  , lstCertState :: !(CertState era)
  }
  deriving (Generic)

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
