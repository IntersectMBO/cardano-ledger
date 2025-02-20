{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.CertState () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.CertState
import Cardano.Ledger.Shelley.CertState
import Data.Coerce (coerce)

instance EraCertState AlonzoEra where
  type CertState AlonzoEra = ShelleyCertState AlonzoEra

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
