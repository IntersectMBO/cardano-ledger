{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Package all the crypto constraints into one place.
module Shelley.Spec.Ledger.Crypto where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.KES
import Cardano.Crypto.VRF
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word (Word8)

class
  ( HashAlgorithm (HASH c),
    DSIGNAlgorithm (DSIGN c),
    KESAlgorithm (KES c),
    VRFAlgorithm (VRF c),
    ContextDSIGN (DSIGN c) ~ (),
    ContextKES (KES c) ~ (),
    ContextVRF (VRF c) ~ (),
    Typeable c
  ) =>
  Crypto c where
  type HASH c :: Type
  type DSIGN c :: Type
  type KES c :: Type
  type VRF c :: Type
  networkMagicId :: proxy c -> Network

data Network =
  Mainnet | Testnet | Offline
  deriving (Eq, Ord, Enum)

networkToWord8 :: Network -> Word8
networkToWord8 = toEnum . fromEnum
