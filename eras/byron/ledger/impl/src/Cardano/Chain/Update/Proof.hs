module Cardano.Chain.Update.Proof
  ( Proof,
    mkProof,
    recoverProof,
  )
where

import Cardano.Chain.Update.Payload (APayload (..), Payload)
import Cardano.Crypto (Hash, hashDecoded, serializeCborHash)
import Cardano.Prelude

-- | Proof that body of update message contains 'Update.Payload'
type Proof = Hash Payload

mkProof :: Payload -> Proof
mkProof = serializeCborHash

recoverProof :: APayload ByteString -> Proof
recoverProof = hashDecoded
