{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Ledger.Signatures where

import           Crypto.Hash (Digest, SHA256)
import qualified Data.ByteArray        as BA
import Data.Monoid (Sum(..))
import           Numeric.Natural       (Natural)

-- | An encoded hash of part of the system.
type Hash = Digest SHA256

-- | A unit of value held by a UTxO.
newtype Value = Value Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

-- |The address of a transaction output, used to identify the owner.
newtype Addr = Addr Hash
  deriving (Show, Eq, Ord)
