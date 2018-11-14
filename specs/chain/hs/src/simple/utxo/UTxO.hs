-- NOTE: this file was copied from the ledger implementation from its
-- 'src/simple/utxo/UTxO.hs' file because I found no way to import it.
--   ~ Marko Dimjašević, Nov 14, 2018

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
module UTxO where

import           Crypto.Hash (Digest, SHA256)
import qualified Data.ByteArray        as BA
import Data.Monoid (Sum(..))
import           Numeric.Natural       (Natural)

type Hash = Digest SHA256

newtype Value = Value Natural
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via (Sum Natural)

-- |The address of a transaction output, used to identify the owner.
newtype Addr = Addr Hash
  deriving (Show, Eq, Ord)
