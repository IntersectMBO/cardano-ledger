{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.KES (
  KESKeyPair (..),
) where

import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (VerKeyKES)

data KESKeyPair c = KESKeyPair
  { kesSignKey :: !(KES.UnsoundPureSignKeyKES (KES c))
  , kesVerKey :: !(VerKeyKES c)
  }

instance Show (KES.VerKeyKES (KES c)) => Show (KESKeyPair c) where
  show (KESKeyPair _ vk) =
    -- showing `SignKeyKES` is impossible for security reasons.
    "KESKeyPair <SignKeyKES> " <> show vk
