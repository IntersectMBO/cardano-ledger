{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Protocol.Crypto.KES (
  KESKeyPair (..),
) where

import Cardano.Crypto.KES (
  UnsoundPureKESAlgorithm (..),
  seedSizeKES,
  unsoundPureDeriveVerKeyKES,
  unsoundPureGenKeyKES,
 )
import qualified Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.Seed
import Cardano.Protocol.Crypto
import Data.Proxy
import Test.Cardano.Ledger.Binary.Arbitrary (genByteString)
import Test.Cardano.Ledger.Common

data KESKeyPair c = KESKeyPair
  { kesSignKey :: !(KES.UnsoundPureSignKeyKES (KES c))
  , kesVerKey :: !(KES.VerKeyKES (KES c))
  }

instance Show (KES.VerKeyKES (KES c)) => Show (KESKeyPair c) where
  show (KESKeyPair _ vk) =
    -- showing `SignKeyKES` is impossible for security reasons.
    "KESKeyPair <SignKeyKES> " <> show vk

-- TODO: upstream into `cardano-base`

-- | Generate a `Seed` with specified number of bytes, which can only be positive.
genSeedN :: HasCallStack => Int -> Gen Seed
genSeedN n
  | n >= 1 = mkSeedFromBytes <$> genByteString n
  | otherwise = error $ "Seed cannot be empty. Supplied " ++ show n ++ " for the size of the seed"

instance Crypto c => Arbitrary (KESKeyPair c) where
  arbitrary = do
    signKey <- unsoundPureGenKeyKES <$> genSeedN (fromIntegral (seedSizeKES (Proxy @(KES c))))
    pure $
      KESKeyPair
        { kesSignKey = signKey
        , kesVerKey = unsoundPureDeriveVerKeyKES signKey
        }
