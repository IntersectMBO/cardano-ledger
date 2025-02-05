{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.VRF (
  VRFKeyPair (..),
) where

import qualified Cardano.Crypto.VRF.Class as VRF
import Cardano.Protocol.Crypto
import Test.Cardano.Ledger.Common

data VRFKeyPair c = VRFKeyPair
  { vrfSignKey :: !(VRF.SignKeyVRF (VRF c))
  , vrfVerKey :: !(VRF.VerKeyVRF (VRF c))
  }

deriving instance
  (Show (VRF.SignKeyVRF (VRF c)), Show (VRF.VerKeyVRF (VRF c))) => Show (VRFKeyPair c)

instance (Crypto c, Arbitrary (VRF.SignKeyVRF (VRF c))) => Arbitrary (VRFKeyPair c) where
  arbitrary = do
    signKey <- arbitrary
    pure $
      VRFKeyPair
        { vrfSignKey = signKey
        , vrfVerKey = VRF.deriveVerKeyVRF signKey
        }
