module Cardano.Chain.UTxO.ValidationMode
  ( TxValidationMode (..),
  )
where

import Cardano.Prelude

--------------------------------------------------------------------------------
-- TxValidationMode
--------------------------------------------------------------------------------

-- | Indicates what sort of transaction validation should be performed.
data TxValidationMode
  = -- | Perform all transaction validations.
    TxValidation
  | -- | Because we've already validated this transaction against some ledger
    -- state, we know that cryptographic validation has passed. However, we
    -- should still perform all of the other non-cryptographic checks since
    -- we're validating against a potentially dfferent ledger state.
    TxValidationNoCrypto
  | -- | No validations should be performed as we have already validated this
    -- transaction against this very same ledger state.
    NoTxValidation
  deriving (Eq, Show)
