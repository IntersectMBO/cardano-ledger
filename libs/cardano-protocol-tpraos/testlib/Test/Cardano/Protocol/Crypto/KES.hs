{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.KES (
  KESKeyPair (..),
) where

import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (VerKeyKES, UnsoundPureSignKeyKES)

data KESKeyPair c = KESKeyPair
  { kesSignKey :: !(UnsoundPureSignKeyKES c)
  , kesVerKey :: !(VerKeyKES c)
  }

-- This will not work with mlocked KES:
--
-- deriving instance
--   (Show (KES.SignKeyKES (KES c)), Show (KES.VerKeyKES (KES c))) => Show (KESKeyPair c)

instance (Show (KES.VerKeyKES (KES c))) => Show (KESKeyPair c) where
  show (KESKeyPair _ vk) =
    "KESKeyPair <SignKeyKES> " <> show vk

-- deriving instance (Show (KES.SignKeyKES (KES c)), Show (KES.VerKeyKES (KES c))) => Show (KESKeyPair c)
