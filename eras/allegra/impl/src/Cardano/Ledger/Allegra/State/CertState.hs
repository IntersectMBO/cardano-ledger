{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.State.CertState () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Shelley.State

instance EraCertState AllegraEra where
  type CertState AllegraEra = ShelleyCertState AllegraEra

  certDStateL = shelleyCertDStateL
  {-# INLINE certDStateL #-}

  certPStateL = shelleyCertPStateL
  {-# INLINE certPStateL #-}

  obligationCertState = shelleyObligationCertState

  certsTotalDepositsTxBody = shelleyCertsTotalDepositsTxBody

  certsTotalRefundsTxBody = shelleyCertsTotalRefundsTxBody
