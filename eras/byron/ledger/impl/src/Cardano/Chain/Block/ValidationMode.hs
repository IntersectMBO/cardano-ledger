{-# LANGUAGE FlexibleContexts #-}

module Cardano.Chain.Block.ValidationMode
  ( BlockValidationMode (..),
    toTxValidationMode,
  )
where

import Cardano.Chain.UTxO.ValidationMode (TxValidationMode (..))
import Cardano.Prelude

--------------------------------------------------------------------------------
-- BlockValidationMode
--------------------------------------------------------------------------------

-- | Indicates what sort of block validation should be performed.
data BlockValidationMode
  = -- | Perform all block validations.
    BlockValidation
  | -- | Perform no block validations.
    NoBlockValidation
  deriving (Eq, Show)

-- | Translate a 'BlockValidationMode' to an appropriate 'TxValidationMode'.
toTxValidationMode :: BlockValidationMode -> TxValidationMode
toTxValidationMode BlockValidation = TxValidation
toTxValidationMode NoBlockValidation = NoTxValidation
