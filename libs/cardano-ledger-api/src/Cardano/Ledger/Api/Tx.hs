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
  module Cardano.Ledger.Api.Tx.AuxData,
  module Cardano.Ledger.Api.Tx.Wits,

  -- * Shelley onwards
  EraTx,
  mkBasicTx,
  bodyTxL,
  witsTxL,
  auxDataTxL,
  validateScript,
  sizeTxF,
  getMinFeeTx,
  setMinFeeTx,

  -- * Alonzo onwards
  AlonzoEraTx,
  isValidTxL,
  IsValid (..),
)
where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..), IsValid (..))
import Cardano.Ledger.Api.Era ()
import Cardano.Ledger.Api.Tx.AuxData
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.Api.Tx.Wits
import Cardano.Ledger.Core (EraTx (..), PParams)
import Lens.Micro ((&), (.~), (^.))

-- | Calculate and update the fee in the transaction until it has the smallest possible
-- value according to the settings in the protocol parameters.
setMinFeeTx :: EraTx era => PParams era -> Tx era -> Tx era
setMinFeeTx pp tx =
  let curMinFee = getMinFeeTx pp tx
      curFee = tx ^. bodyTxL ^. feeTxBodyL
      modifiedTx = tx & bodyTxL . feeTxBodyL .~ curMinFee
   in if curFee == curMinFee
        then tx
        else setMinFeeTx pp modifiedTx
