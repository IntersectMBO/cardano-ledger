{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.State.CertState () where

import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.State.Account ()
import Cardano.Ledger.Shelley.State

instance EraCertState MaryEra where
  type CertState MaryEra = ShelleyCertState MaryEra

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody
