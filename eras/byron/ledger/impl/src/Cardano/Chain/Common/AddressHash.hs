module Cardano.Chain.Common.AddressHash (
  AddressHash,
  addressHash,
  unsafeAddressHash,
)
where

import Cardano.Crypto.Hashing (AbstractHash, abstractHashFromDigest)
import Cardano.Ledger.Binary (EncCBOR, byronProtVer, serialize)
import Cardano.Prelude
import Crypto.Hash (Blake2b_224, Digest, SHA3_256)
import qualified Crypto.Hash as CryptoHash

-- | Hash used to identify address.
type AddressHash = AbstractHash Blake2b_224

unsafeAddressHash :: EncCBOR a => a -> AddressHash b
unsafeAddressHash = abstractHashFromDigest . secondHash . firstHash
  where
    firstHash :: EncCBOR a => a -> Digest SHA3_256
    firstHash = CryptoHash.hashlazy . serialize byronProtVer
    secondHash :: Digest SHA3_256 -> Digest Blake2b_224
    secondHash = CryptoHash.hash

addressHash :: EncCBOR a => a -> AddressHash a
addressHash = unsafeAddressHash
