module Cardano.Ledger.Api.Tx.In (
  -- * Transaction input
  TxIn (..),

  -- ** Transaction ID
  TxId (..),
  mkTxInPartial,

  -- ** Transaction index
  TxIx,
  mkTxIx,
  txIxToInt,
  txIxFromIntegral,
  mkTxIxPartial,
)
where

import Cardano.Ledger.BaseTypes (
  TxIx,
  mkTxIx,
  mkTxIxPartial,
  txIxFromIntegral,
  txIxToInt,
 )
import Cardano.Ledger.TxIn (
  TxId (..),
  TxIn (TxIn),
  mkTxInPartial,
 )
