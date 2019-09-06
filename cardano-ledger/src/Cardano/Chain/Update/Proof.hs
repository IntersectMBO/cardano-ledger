module Cardano.Chain.Update.Proof
  ( Proof
  , mkProof
  , recoverProof
  )
where

import Cardano.Chain.Update.Payload (Payload(..))
import Cardano.Crypto (Hash, hash)


-- | Proof that body of update message contains 'Update.Payload'
type Proof = Hash Payload

mkProof :: Payload -> Proof
mkProof = hash

recoverProof :: Payload -> Proof
recoverProof = hash
