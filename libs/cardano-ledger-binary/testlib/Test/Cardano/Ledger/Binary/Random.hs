{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Random (
  QC (..),
  mkDummyHash,
  mkHashStdGen,
)
where

import Cardano.Crypto.Hash.Class (Hash, HashAlgorithm, hashToBytesShort)
import Cardano.Crypto.Hash.Short (Blake2bPrefix)
import Cardano.Ledger.Binary (EncCBOR (encCBOR), hashWithEncoder, shelleyProtVer)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Coerce (coerce)
import Data.Primitive.ByteArray (ByteArray (ByteArray), indexByteArray)
import System.Random.Stateful (
  StatefulGen (..),
  StdGen,
  mkStdGen,
  runStateGen_,
 )
import Test.QuickCheck.Gen (Gen (MkGen))

-- | This is a pseudo random number generator used by QuickCheck and allows to use
-- @random@'s stateful interface to work dierctly in `Gen` monad. This comes from an
-- unmerged QuickCheck PR: https://github.com/nick8325/quickcheck/pull/333
data QC = QC

instance StatefulGen QC Gen where
  uniformWord32 QC = MkGen (\r _n -> runStateGen_ r uniformWord32)
  {-# INLINE uniformWord32 #-}
  uniformWord64 QC = MkGen (\r _n -> runStateGen_ r uniformWord64)
  {-# INLINE uniformWord64 #-}
  uniformShortByteString k QC =
    MkGen (\r _n -> runStateGen_ r (uniformShortByteString k))
  {-# INLINE uniformShortByteString #-}

-- | It is possible to use a hash of a binary representation of any type as a source of
-- randomness, since hash value by its definiteion is uniformly distributed.
mkDummyHash :: forall h a b. (HashAlgorithm h, EncCBOR a) => a -> Hash h b
mkDummyHash = coerce . hashWithEncoder @h shelleyProtVer encCBOR

-- | Use a hash of the binary representation of a type as a seed to construct `StdGen`,
-- that can be further used to generate random values.
mkHashStdGen :: EncCBOR x => x -> StdGen
mkHashStdGen x =
  case hashToBytesShort $ mkDummyHash @(Blake2bPrefix 8) x of
    SBS ba -> mkStdGen (indexByteArray (ByteArray ba) 0)
