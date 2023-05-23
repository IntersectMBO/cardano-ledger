{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxCert () where

import Cardano.Ledger.Allegra.Era
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley.TxCert

instance Crypto c => EraTxCert (AllegraEra c) where
  {-# SPECIALIZE instance EraTxCert (AllegraEra StandardCrypto) #-}

  type TxCert (AllegraEra c) = ShelleyTxCert (AllegraEra c)

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkRegPoolTxCert = ShelleyTxCertPool . RegPool

  getRegPoolTxCert (ShelleyTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ShelleyTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ShelleyTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (AllegraEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (AllegraEra StandardCrypto) #-}

  mkShelleyTxCertDeleg = ShelleyTxCertDelegCert

  getShelleyTxCertDeleg (ShelleyTxCertDelegCert c) = Just c
  getShelleyTxCertDeleg _ = Nothing

  mkTxCertGenesisDeleg = ShelleyTxCertGenesisDeleg

  getTxCertGenesisDeleg (ShelleyTxCertGenesisDeleg c) = Just c
  getTxCertGenesisDeleg _ = Nothing

  mkTxCertMir = ShelleyTxCertMir

  getTxCertMir (ShelleyTxCertMir c) = Just c
  getTxCertMir _ = Nothing
