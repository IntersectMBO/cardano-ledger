-- \* Building and inspecting transactions
--

-- | Transaction building and inspecting relies heavily on lenses (`microlens`). Therefore, some
-- familiarity with those is necessary. However, you can probably go a long way by simply
-- looking at the examples and try to go from there.
--
-- Here's an example on how to build a very simple Babbage era unbalanced transaction using the
-- provided interface.
--
-- >>> :set -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import qualified Data.Sequence.Strict as StrictSeq
-- >>> import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
-- >>> import Lens.Micro
-- prop> \txOut ->
--     let
--         -- Defining a Babbage era transaction body with a single random transaction output
--         txBody = mkBasicTxBody
--                & outputsTxBodyL <>~ StrictSeq.singleton (txOut @Babbage)
--         -- Defining a basic transaction with our transaction body
--         tx = mkBasicTx txBody
--      in
--         -- We verify that the transaction's outputs contains our single random output
--         tx ^. bodyTxL . outputsTxBodyL == StrictSeq.singleton txOut
module Cardano.Ledger.Api.Tx
  ( -- | Building and inspecting transaction bodies
    module Cardano.Ledger.Api.Tx.Body,
    module Cardano.Ledger.Api.Tx.AuxData,
    module Cardano.Ledger.Api.Tx.Wits,
    EraTx (..),

    -- * Shelley, Allegra and Mary Era
    ShelleyTx,

    -- * Alonzo and Babbage Era
    AlonzoTx,
    AlonzoEraTx (..),
  )
where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), AlonzoTx)
import Cardano.Ledger.Api.Tx.AuxData
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.Api.Tx.Wits
import Cardano.Ledger.Core (EraTx (..))
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
