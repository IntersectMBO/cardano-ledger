{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.CertState () where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.CertState
import Cardano.Ledger.Shelley.CertState
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
