

-- | Unsafe operations that are intended to be used in tests, but not in the executable specifications.
module Ledger.Core.Unsafe where

import           Ledger.Core (SKey (SKey), Sig (Sig), VKey (VKey), VKeyGenesis, owner, sign,
                     unVKeyGenesis)


-- | Extract the verifying key of a signature. This is useful when elaborating
-- an abstract signature into a concrete one.
signatureVKey :: Sig a -> VKey
signatureVKey (Sig _data aOwner) = VKey aOwner


-- | Get the signing key from the verification key. We use this in the
-- generators, where we need to generate signed data for a given verification
-- key (e.g. one that appears in the delegation map) for which we don't have
-- the signing key.
skey :: VKey -> SKey
skey = SKey . owner


-- | Sign using a genesis verifying key.
--
-- See 'skey' for details about situations where this function is used.
signWithGenesisKey :: VKeyGenesis -> a -> Sig a
signWithGenesisKey vkg = sign (skey (unVKeyGenesis vkg))
