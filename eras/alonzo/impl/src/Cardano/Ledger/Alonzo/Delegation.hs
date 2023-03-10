{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Delegation () where

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.Delegation

instance Crypto c => EraDCert (AlonzoEra c) where
  {-# SPECIALIZE instance EraDCert (AlonzoEra StandardCrypto) #-}

  type DCert (AlonzoEra c) = ShelleyDCert (AlonzoEra c)

  mkDCertDeleg = ShelleyDCertDeleg

  getDCertDeleg (ShelleyDCertDeleg c) = Just c
  getDCertDeleg _ = Nothing

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

instance Crypto c => ShelleyEraDCert (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (AlonzoEra StandardCrypto) #-}

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing
