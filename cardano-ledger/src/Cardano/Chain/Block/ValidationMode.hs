{-# LANGUAGE FlexibleContexts #-}

module Cardano.Chain.Block.ValidationMode
  ( BlockValidationMode (..)
  , orThrowErrorBVM
  , toTxValidationMode
  , whenBlockValidation
  ) where

import Cardano.Prelude

import Cardano.Chain.UTxO.ValidationMode (TxValidationMode (..))

--------------------------------------------------------------------------------
-- BlockValidationMode
--------------------------------------------------------------------------------

-- | Indicates what sort of block validation should be performed.
data BlockValidationMode
  = BlockValidation
  -- ^ Perform all block validations.
  | NoBlockValidation
  -- ^ Perform no block validations.
  deriving (Eq, Show)

-- | Perform an action only when in the 'BlockValidation' mode. Otherwise, do
-- nothing.
whenBlockValidation
  :: MonadError err m
  => BlockValidationMode
  -> m ()
  -> m ()
whenBlockValidation BlockValidation action = action
whenBlockValidation _ _ = pure ()

orThrowErrorBVM
  :: (MonadError e m, MonadReader BlockValidationMode m)
  => Bool
  -> e
  -> m ()
orThrowErrorBVM condition err = do
  bvm <- ask
  unless (bvm == NoBlockValidation || condition) (throwError err)

infix 1 `orThrowErrorBVM`

-- | Translate a 'BlockValidationMode' to an appropriate 'TxValidationMode'.
toTxValidationMode :: BlockValidationMode -> TxValidationMode
toTxValidationMode BlockValidation   = TxValidation
toTxValidationMode NoBlockValidation = NoTxValidation
