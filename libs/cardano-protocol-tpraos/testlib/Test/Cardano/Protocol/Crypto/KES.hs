{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.KES (
  KESKeyPair (..),
) where

import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Protocol.Crypto

data KESKeyPair c = KESKeyPair
  { kesSignKey :: !(KES.UnsoundPureSignKeyKES (KES c))
  , kesVerKey :: !(KES.VerKeyKES (KES c))
  }

instance Show (KES.VerKeyKES (KES c)) => Show (KESKeyPair c) where
  show (KESKeyPair _ vk) =
    -- showing `SignKeyKES` is impossible for security reasons.
    "KESKeyPair <SignKeyKES> " <> show vk
