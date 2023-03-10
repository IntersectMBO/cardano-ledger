{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Delegation () where

import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Delegation

instance Crypto c => EraDCert (AllegraEra c) where
  {-# SPECIALIZE instance EraDCert (AllegraEra StandardCrypto) #-}

  type DCert (AllegraEra c) = ShelleyDCert (AllegraEra c)

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

instance Crypto c => ShelleyEraDCert (AllegraEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (AllegraEra StandardCrypto) #-}

  mkShelleyDCertDeleg = ShelleyDCertDelegCert

  getShelleyDCertDeleg (ShelleyDCertDelegCert c) = Just c
  getShelleyDCertDeleg _ = Nothing

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing
