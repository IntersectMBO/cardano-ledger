{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.VRF (
  VRFKeyPair (..),
) where

import qualified Cardano.Crypto.VRF.Class as VRF
import Cardano.Protocol.HeaderCrypto
import Cardano.Protocol.HeaderKeys (
  SignKeyVRF,
  VerKeyVRF,
 )

data VRFKeyPair c = VRFKeyPair
  { vrfSignKey :: !(SignKeyVRF c)
  , vrfVerKey :: !(VerKeyVRF c)
  }

deriving instance (Show (VRF.SignKeyVRF (VRF c)), Show (VRF.VerKeyVRF (VRF c))) => Show (VRFKeyPair c)
