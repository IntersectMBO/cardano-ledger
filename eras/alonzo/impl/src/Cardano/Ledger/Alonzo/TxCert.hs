{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.TxCert () where

import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Shelley.TxCert

instance Crypto c => EraTxCert (AlonzoEra c) where
  {-# SPECIALIZE instance EraTxCert (AlonzoEra StandardCrypto) #-}

  type TxCert (AlonzoEra c) = ShelleyTxCert (AlonzoEra c)

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkRegPoolTxCert = ShelleyTxCertPool . RegPool

  getRegPoolTxCert (ShelleyTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ShelleyTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ShelleyTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (AlonzoEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (AlonzoEra StandardCrypto) #-}

  mkRegTxCert = ShelleyTxCertDelegCert . ShelleyRegCert

  getRegTxCert (ShelleyTxCertDelegCert (ShelleyRegCert c)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert = ShelleyTxCertDelegCert . ShelleyUnRegCert

  getUnRegTxCert (ShelleyTxCertDelegCert (ShelleyUnRegCert c)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ShelleyTxCertDelegCert $ ShelleyDelegCert c kh

  getDelegStakeTxCert (ShelleyTxCertDelegCert (ShelleyDelegCert c kh)) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkTxCertGenesisDeleg = ShelleyTxCertGenesisDeleg

  getTxCertGenesisDeleg (ShelleyTxCertGenesisDeleg c) = Just c
  getTxCertGenesisDeleg _ = Nothing

  mkTxCertMir = ShelleyTxCertMir

  getTxCertMir (ShelleyTxCertMir c) = Just c
  getTxCertMir _ = Nothing
