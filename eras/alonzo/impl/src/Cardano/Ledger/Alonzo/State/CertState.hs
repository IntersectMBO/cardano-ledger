{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.State.CertState () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Shelley.State
import Data.Coerce (coerce)

instance EraCertState AlonzoEra where
  type CertState AlonzoEra = ShelleyCertState AlonzoEra

  upgradeCertState = coerce

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody
