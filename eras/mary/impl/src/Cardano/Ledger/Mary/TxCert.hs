{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxCert () where

import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Shelley.TxCert (
  EraTxCert (..),
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

  mkTxCertPool = ShelleyTxCertPool

  getRegPoolTxCert (ShelleyTxCertPool c) = Just c
  getRegPoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (MaryEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (MaryEra StandardCrypto) #-}

  mkShelleyTxCertDeleg = ShelleyTxCertDelegCert

  getShelleyTxCertDeleg (ShelleyTxCertDelegCert c) = Just c
  getShelleyTxCertDeleg _ = Nothing

  mkTxCertGenesisDeleg = ShelleyTxCertGenesisDeleg

  getTxCertGenesisDeleg (ShelleyTxCertGenesisDeleg c) = Just c
  getTxCertGenesisDeleg _ = Nothing

  mkTxCertMir = ShelleyTxCertMir

  getTxCertMir (ShelleyTxCertMir c) = Just c
  getTxCertMir _ = Nothing
