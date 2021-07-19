module Cardano.Chain.Common.AddressHash
  ( AddressHash,
    addressHash,
    unsafeAddressHash,
  )
where

import Cardano.Binary (ToCBOR, serialize)
import Cardano.Crypto.Hashing (AbstractHash, abstractHashFromDigest)
import Cardano.Prelude
import Crypto.Hash (Blake2b_224, Digest, SHA3_256)
import qualified Crypto.Hash as CryptoHash

-- | Hash used to identify address.
type AddressHash = AbstractHash Blake2b_224

unsafeAddressHash :: ToCBOR a => a -> AddressHash b
unsafeAddressHash = abstractHashFromDigest . secondHash . firstHash
  where
    firstHash :: ToCBOR a => a -> Digest SHA3_256
    firstHash = CryptoHash.hashlazy . serialize
    secondHash :: Digest SHA3_256 -> Digest Blake2b_224
    secondHash = CryptoHash.hash

addressHash :: ToCBOR a => a -> AddressHash a
addressHash = unsafeAddressHash
