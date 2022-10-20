module Cardano.Ledger.Shelley
  ( Shelley,
    ShelleyEra,
    ShelleyTx,
    ShelleyTxOut,
    ShelleyTxBody,
    Value,
    Script,
    TxAuxData,
    ShelleyPParams,
    TxWits,
    nativeMultiSigTag,
    -- Deprecated
    -- PParams,
    Shelley.Tx,
    Shelley.TxOut,
    Shelley.TxBody,
  )
where

import Cardano.Ledger.Core
  ( EraTxAuxData (TxAuxData),
    EraTxWits (TxWits),
    Script,
    Value,
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Cardano.Ledger.Shelley.Rules ()
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    ShelleyTxBody,
    ShelleyTxOut,
    nativeMultiSigTag,
  )
import qualified Cardano.Ledger.Shelley.Tx as Shelley (Tx, TxBody, TxOut)

type Shelley = ShelleyEra StandardCrypto
