module Cardano.Ledger.Shelley (
  Shelley,
  ShelleyEra,
  ShelleyTx,
  ShelleyTxOut,
  TxBody (..),
  Tx (..),
  ShelleyTxAuxData,
  nativeMultiSigTag,
  hardforkAllegraAggregatedRewards,
  hardforkAlonzoAllowMIRTransfer,
  hardforkAlonzoValidatePoolRewardAccountNetID,
  hardforkBabbageForgoRewardPrefilter,
) where

import Cardano.Ledger.Shelley.Era (
  ShelleyEra,
  hardforkAllegraAggregatedRewards,
  hardforkAlonzoAllowMIRTransfer,
  hardforkAlonzoValidatePoolRewardAccountNetID,
  hardforkBabbageForgoRewardPrefilter,
 )
import Cardano.Ledger.Shelley.Genesis ()
import Cardano.Ledger.Shelley.Governance ()
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.Shelley.Rules ()
import Cardano.Ledger.Shelley.Scripts (nativeMultiSigTag)
import Cardano.Ledger.Shelley.Translation ()
import Cardano.Ledger.Shelley.Tx (ShelleyTx, Tx (..))
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Ledger.Shelley.TxBody (TxBody (..))
import Cardano.Ledger.Shelley.TxOut (ShelleyTxOut)
import Cardano.Ledger.Shelley.UTxO ()

type Shelley = ShelleyEra

{-# DEPRECATED Shelley "In favor of `ShelleyEra`" #-}
