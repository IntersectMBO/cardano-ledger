module Cardano.Chain.UTxO.ValidationMode
  ( TxValidationMode (..)
  , whenTxValidation
  ) where

import Cardano.Prelude

--------------------------------------------------------------------------------
-- TxValidationMode
--------------------------------------------------------------------------------

-- | Indicates what sort of transaction validation should be performed.
data TxValidationMode
  = TxValidation
  -- ^ Perform all transaction validations.
  | TxValidationNoCrypto
  -- ^ Because we've already validated this transaction against some ledger
  -- state, we know that cryptographic validation has passed. However, we
  -- should still perform all of the other non-cryptographic checks since
  -- we're validating against a potentially dfferent ledger state.
  | NoTxValidation
  -- ^ No validations should be performed as we have already validated this
  -- transaction against this very same ledger state.
  deriving (Eq, Show)

-- | Perform an action only when in the 'TxValidation' mode. Otherwise, do
-- nothing.
whenTxValidation
  :: MonadError err m
  => TxValidationMode
  -> m ()
  -> m ()
whenTxValidation TxValidation action = action
whenTxValidation _ _ = pure ()
