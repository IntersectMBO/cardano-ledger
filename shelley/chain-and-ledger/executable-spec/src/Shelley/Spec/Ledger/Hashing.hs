{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Hashing
  ( HashAnnotated (..),
    EraIndependentTx,
    EraIndependentTxBody,
    EraIndependentBlockBody,
    EraIndependentWitVKey,
  )
where

import Cardano.Binary (ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.Era (Crypto (..))
import Data.Kind

--  Uninhabited types used as HashIndex ranges for HashAnnotated instances
data EraIndependentTx

data EraIndependentTxBody

data EraIndependentBlockBody

data EraIndependentWitVKey

class Era e => HashAnnotated a e | a -> e where
  type HashIndex a :: Type
  hashAnnotated :: a -> Hash.Hash (HASH (Crypto e)) (HashIndex a)
  default hashAnnotated :: ToCBOR a => a -> Hash.Hash (HASH (Crypto e)) (HashIndex a)
  hashAnnotated x = Hash.castHash (Hash.hashWithSerialiser @(HASH (Crypto e)) toCBOR x)
