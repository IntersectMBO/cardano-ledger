{-# LANGUAGE FlexibleContexts #-}

module Cardano.Chain.Block.ValidationMode
  ( BlockValidationMode (..)
  , toTxValidationMode
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

-- | Translate a 'BlockValidationMode' to an appropriate 'TxValidationMode'.
toTxValidationMode :: BlockValidationMode -> TxValidationMode
toTxValidationMode BlockValidation   = TxValidation
toTxValidationMode NoBlockValidation = NoTxValidation
