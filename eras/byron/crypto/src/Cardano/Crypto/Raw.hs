{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Crypto.Raw
  ( Raw (..),
  )
where

import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Prelude

-- | A wrapper over 'ByteString' for signalling that a bytestring should be
--   processed as a sequence of bytes, not as a separate entity. It's used in
--   crypto and binary code.
newtype Raw = Raw ByteString
  deriving (Eq, Ord, Show, NFData, FromCBOR, ToCBOR)
