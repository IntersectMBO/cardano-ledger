{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.Delegation () where

import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Delegation

instance Crypto c => EraDCert (BabbageEra c) where
  {-# SPECIALIZE instance EraDCert (BabbageEra StandardCrypto) #-}

  type DCert (BabbageEra c) = ShelleyDCert (BabbageEra c)

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

instance Crypto c => ShelleyEraDCert (BabbageEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (BabbageEra StandardCrypto) #-}

  mkShelleyDCertDeleg = ShelleyDCertDelegCert

  getShelleyDCertDeleg (ShelleyDCertDelegCert c) = Just c
  getShelleyDCertDeleg _ = Nothing

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing
