{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Delegation () where

import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Era
import Cardano.Ledger.Shelley.Delegation

instance Crypto c => EraDCert (MaryEra c) where
  {-# SPECIALIZE instance EraDCert (MaryEra StandardCrypto) #-}

  type DCert (MaryEra c) = ShelleyDCert (MaryEra c)

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

instance Crypto c => ShelleyEraDCert (MaryEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (MaryEra StandardCrypto) #-}

  mkShelleyDCertDeleg = ShelleyDCertDelegCert

  getShelleyDCertDeleg (ShelleyDCertDelegCert c) = Just c
  getShelleyDCertDeleg _ = Nothing

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing
