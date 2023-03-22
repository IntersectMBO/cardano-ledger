module Cardano.Ledger.Api.Tx.Wits (
  -- * Shelley onwards
  EraTxWits (TxWits),
  mkBasicTxWits,

  -- ** Address witness
  addrTxWitsL,
  KeyRole (Witness),

  -- *** WitVKey
  WitVKey (WitVKey),
  witVKeyBytes,
  witVKeyHash,

  -- ** Byron address witness
  bootAddrTxWitsL,
  BootstrapWitness,

  -- ** Script address witness
  scriptTxWitsL,

  -- * Alonzo onwards
  AlonzoEraTxWits,
  datsTxWitsL,
  hashDataTxWitsL,
  TxDats (..),
  unTxDats,
  rdmrsTxWitsL,
  Redeemers (..),
  unRedeemers,
  RdmrPtr (..),
  Tag (..),
)
where

import Cardano.Ledger.Alonzo.Scripts (Tag (..))
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits,
  RdmrPtr (..),
  Redeemers (..),
  TxDats (..),
  datsTxWitsL,
  hashDataTxWitsL,
  rdmrsTxWitsL,
  unRedeemers,
  unTxDats,
 )
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Core (EraTxWits (..))
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (WitVKey), witVKeyBytes, witVKeyHash)
