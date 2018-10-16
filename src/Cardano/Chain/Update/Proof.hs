module Cardano.Chain.Update.Proof
       ( Proof
       , mkProof
       ) where

import           Cardano.Crypto (Hash, hash)

import           Cardano.Chain.Update.Payload (Payload)


-- | Proof that body of update message contains 'Update.Payload'
type Proof = Hash Payload

mkProof :: Payload -> Proof
mkProof = hash
