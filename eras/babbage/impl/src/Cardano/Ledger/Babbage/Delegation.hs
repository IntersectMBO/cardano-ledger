{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Delegation () where

import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Delegation

instance Crypto c => EraDCert (AllegraEra c) where
  {-# SPECIALIZE instance EraDCert (AllegraEra StandardCrypto) #-}

  type DCert (AllegraEra c) = ShelleyDCert (AllegraEra c)

  mkDCertDeleg = ShelleyDCertDeleg

  getDCertDeleg (ShelleyDCertDeleg c) = Just c
  getDCertDeleg _ = Nothing

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

instance Crypto c => ShelleyEraDCert (AllegraEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (AllegraEra StandardCrypto) #-}

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing
