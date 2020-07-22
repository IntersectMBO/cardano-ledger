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
import Shelley.Spec.Ledger.Crypto (Crypto (..))

class Crypto c => HashAnnotated a c | a -> c where
  hashAnnotated :: a -> Hash.Hash (HASH c) a
  default hashAnnotated :: ToCBOR a => a -> Hash.Hash (HASH c) a
  hashAnnotated = Hash.hashWithSerialiser @(HASH c) toCBOR
