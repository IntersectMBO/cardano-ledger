{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.KES (
  KESKeyPair (..),
) where

import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  SignKeyKES,
  VerKeyKES,
 )

data KESKeyPair c = KESKeyPair
  { kesSignKey :: !(SignKeyKES c)
  , kesVerKey :: !(VerKeyKES c)
  }

deriving instance (Show (KES.SignKeyKES (KES c)), Show (KES.VerKeyKES (KES c))) => Show (KESKeyPair c)
