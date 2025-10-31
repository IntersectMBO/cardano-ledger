module Cardano.Ledger.Api.Tx.In (
  -- * Transaction input
  TxIn (..),
  mkTxInPartial,
  mkCollateralTxIn,

  -- ** Transaction ID
  TxId (..),

  -- ** Transaction index
  TxIx,
  txIxToInt,
  txIxFromIntegral,
  mkTxIxPartial,
) where

import Cardano.Ledger.Babbage.Collateral (mkCollateralTxIn)
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
