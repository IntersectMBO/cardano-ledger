{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Transaction building and inspecting relies heavily on lenses (`microlens`). Therefore, some
-- familiarity with those is necessary. However, you can probably go a long way by simply
-- looking at the examples and try to go from there.
--
-- Let's start by defining the GHC extensions and imports.
--
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> import qualified Data.Sequence.Strict as StrictSeq
-- >>> import Cardano.Ledger.Api.Era (Babbage)
-- >>> import Lens.Micro
-- >>> import Test.Cardano.Ledger.Babbage.Arbitrary ()
--
-- Here's an example on how to build a Babbage era unbalanced transaction containing a single
-- transaction output using the provided interface.
--
-- >>> :{
-- quickCheck $ \(txOut :: TxOut Babbage) ->
--     let
--         -- Defining a Babbage era transaction body with a single random transaction output
--         txBody = mkBasicTxBody
--                & outputsTxBodyL <>~ StrictSeq.singleton txOut
--         -- Defining a basic transaction with our transaction body
--         tx = mkBasicTx txBody
--      in
--         -- We verify that the transaction's outputs contains our single random output
--         tx ^. bodyTxL . outputsTxBodyL == StrictSeq.singleton txOut
-- :}
-- +++ OK, passed 100 tests.
module Cardano.Ledger.Api.Tx (
  -- | Building and inspecting transaction bodies
  module Cardano.Ledger.Api.Tx.Body,
  module Cardano.Ledger.Api.Tx.Cert,
  module Cardano.Ledger.Api.Tx.AuxData,
  module Cardano.Ledger.Api.Tx.Wits,

  -- * Shelley onwards
  EraTx (Tx),
  mkBasicTx,
  bodyTxL,
  witsTxL,
  auxDataTxL,
  sizeTxF,
  getMinFeeTx,
  setMinFeeTx,
  estimateMinFeeTx,

  -- * Alonzo onwards
  AlonzoEraTx,
  isValidTxL,
  IsValid (..),

  -- ** Execution units
  evalTxExUnits,
  RedeemerReport,
  evalTxExUnitsWithLogs,
  RedeemerReportWithLogs,
  TransactionScriptFailure (..),
)
where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Api.Scripts.ExUnits (
  RedeemerReport,
  RedeemerReportWithLogs,
  TransactionScriptFailure (..),
  evalTxExUnits,
  evalTxExUnitsWithLogs,
 )
import Cardano.Ledger.Api.Tx.AuxData
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.Api.Tx.Cert
import Cardano.Ledger.Api.Tx.Wits
import Cardano.Ledger.Core (EraTx (..))
import Cardano.Ledger.Tools (estimateMinFeeTx, setMinFeeTx)
