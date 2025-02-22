{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.State.CertState () where

import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.State
import Data.Coerce (coerce)

instance EraCertState ConwayEra where
  type CertState ConwayEra = ShelleyCertState ConwayEra

  mkCertState = mkShelleyCertState

  upgradeCertState = coerce

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certVStateL = shelleyCertVStateL
  {-# INLINE certVStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody
