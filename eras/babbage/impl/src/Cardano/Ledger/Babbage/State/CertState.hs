{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.State.CertState () where

import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Shelley.State

instance EraCertState BabbageEra where
  type CertState BabbageEra = ShelleyCertState BabbageEra

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody
