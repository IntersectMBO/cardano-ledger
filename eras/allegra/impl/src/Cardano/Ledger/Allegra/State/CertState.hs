{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.State.CertState () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.State
import Data.Coerce (coerce)

instance EraCertState AllegraEra where
  type CertState AllegraEra = ShelleyCertState AllegraEra

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
