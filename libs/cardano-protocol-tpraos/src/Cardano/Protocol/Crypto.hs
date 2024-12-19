{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Package all the crypto constraints into one place.
module Cardano.Protocol.Crypto (
  Crypto (..),
  StandardCrypto,
  VRFVerKeyHash (..),
  KeyRoleVRF (..),
  hashVerKeyVRF,
) where

import Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto.Internal
import Cardano.Ledger.Hashes

hashVerKeyVRF :: Crypto c => VRF.VerKeyVRF (VRF c) -> VRFVerKeyHash (r :: KeyRoleVRF)
hashVerKeyVRF = VRFVerKeyHash . Hash.castHash . VRF.hashVerKeyVRF
