module Cardano.Ledger.Api.Tx.In (
  -- * Transaction input
  TxIn (..),

  -- ** Transaction ID
  TxId (..),
  mkTxInPartial,

  -- ** Transaction index
  TxIx,
  txIxToInt,
  txIxFromIntegral,
  mkTxIxPartial,
)
where

import Cardano.Ledger.BaseTypes (
  TxIx,
  mkTxIxPartial,
  txIxFromIntegral,
  txIxToInt,
 )
import Cardano.Ledger.TxIn (
  TxId (..),
  TxIn (TxIn),
  mkTxInPartial,
 )
