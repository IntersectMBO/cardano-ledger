{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxCert () where

import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Shelley.TxCert (
  EraTxCert (..),
  PoolCert (..),
  ShelleyDelegCert (..),
  ShelleyEraTxCert (..),
  ShelleyTxCert (..),
  getScriptWitnessShelleyTxCert,
  getVKeyWitnessShelleyTxCert,
  shelleyTotalDepositsTxCerts,
  shelleyTotalRefundsTxCerts,
  upgradeShelleyTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
 )

instance Crypto c => EraTxCert (MaryEra c) where
  {-# SPECIALIZE instance EraTxCert (MaryEra StandardCrypto) #-}

  type TxCert (MaryEra c) = ShelleyTxCert (MaryEra c)

  upgradeTxCert = Right . upgradeShelleyTxCert

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkRegPoolTxCert = ShelleyTxCertPool . RegPool

  getRegPoolTxCert (ShelleyTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ShelleyTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ShelleyTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

  lookupRegStakeTxCert = \case
    RegTxCert c -> Just c
    _ -> Nothing
  lookupUnRegStakeTxCert = \case
    UnRegTxCert c -> Just c
    _ -> Nothing

  getTotalDepositsTxCerts = shelleyTotalDepositsTxCerts

  getTotalRefundsTxCerts pp lookupStakeDeposit _ = shelleyTotalRefundsTxCerts pp lookupStakeDeposit

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

  mkGenesisDelegTxCert = ShelleyTxCertGenesisDeleg

  getGenesisDelegTxCert (ShelleyTxCertGenesisDeleg c) = Just c
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = ShelleyTxCertMir

  getMirTxCert (ShelleyTxCertMir c) = Just c
  getMirTxCert _ = Nothing
