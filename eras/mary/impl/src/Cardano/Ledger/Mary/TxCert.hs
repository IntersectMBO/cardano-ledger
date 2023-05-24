{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxCert () where

import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.TxCert (
  EraTxCert (..),
  PoolCert (..),
  ShelleyDelegCert (..),
  ShelleyEraTxCert (..),
  ShelleyTxCert (..),
  getScriptWitnessShelleyTxCert,
  getVKeyWitnessShelleyTxCert,
 )

instance Crypto c => EraTxCert (MaryEra c) where
  {-# SPECIALIZE instance EraTxCert (MaryEra StandardCrypto) #-}

  type TxCert (MaryEra c) = ShelleyTxCert (MaryEra c)

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkRegPoolTxCert = ShelleyTxCertPool . RegPool

  getRegPoolTxCert (ShelleyTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ShelleyTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ShelleyTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (MaryEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (MaryEra StandardCrypto) #-}

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
