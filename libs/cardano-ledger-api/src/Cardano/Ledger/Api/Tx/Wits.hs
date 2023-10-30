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

  -- ** Script witness
  scriptTxWitsL,
  hashScriptTxWitsL,

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
  AlonzoRedeemerPurpose (..),
)
where

import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoRedeemerPurpose (..)
  )
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
import Cardano.Ledger.Core (EraTxWits (..), hashScriptTxWitsL)
import Cardano.Ledger.Keys (KeyRole (Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey (WitVKey), witVKeyBytes, witVKeyHash)
