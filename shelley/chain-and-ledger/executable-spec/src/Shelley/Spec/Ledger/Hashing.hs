{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.Hashing
  ( HashAnnotated (..),
  )
where

import Cardano.Binary (ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Era (Crypto (..))

class Era e => HashAnnotated a e | a -> e where
  hashAnnotated :: a -> Hash.Hash (HASH (Crypto e)) a
  default hashAnnotated :: ToCBOR a => a -> Hash.Hash (HASH (Crypto e)) a
  hashAnnotated = Hash.hashWithSerialiser @(HASH (Crypto e)) toCBOR
