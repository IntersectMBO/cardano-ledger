{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxCert () where

import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.TxCert

instance Crypto c => EraTxCert (BabbageEra c) where
  {-# SPECIALIZE instance EraTxCert (BabbageEra StandardCrypto) #-}

  type TxCert (BabbageEra c) = ShelleyTxCert (BabbageEra c)

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkRegPoolTxCert = ShelleyTxCertPool . RegPool

  getRegPoolTxCert (ShelleyTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ShelleyTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ShelleyTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (BabbageEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (BabbageEra StandardCrypto) #-}

  mkShelleyTxCertDeleg = ShelleyTxCertDelegCert

  getShelleyTxCertDeleg (ShelleyTxCertDelegCert c) = Just c
  getShelleyTxCertDeleg _ = Nothing

  mkTxCertGenesisDeleg = ShelleyTxCertGenesisDeleg

  getTxCertGenesisDeleg (ShelleyTxCertGenesisDeleg c) = Just c
  getTxCertGenesisDeleg _ = Nothing

  mkTxCertMir = ShelleyTxCertMir

  getTxCertMir (ShelleyTxCertMir c) = Just c
  getTxCertMir _ = Nothing
