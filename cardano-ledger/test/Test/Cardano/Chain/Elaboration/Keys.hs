module Test.Cardano.Chain.Elaboration.Keys
  ( elaborateKeyPair
  , vKeyPair
  , elaborateVKey
  , elaborateVKeyGenesis
  )
where

import Cardano.Prelude

import qualified Data.ByteString as BS
import Data.ByteString.Builder (integerDec, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL

import Cardano.Crypto.Signing (VerificationKey, SigningKey, deterministicKeyGen)
import Ledger.Core
  ( KeyPair
  , Owner(Owner)
  , VKey(VKey)
  , VKeyGenesis(VKeyGenesis)
  , keyPair
  , owner
  , sKey
  )

elaborateKeyPair :: KeyPair -> (VerificationKey, SigningKey)
elaborateKeyPair kp = deterministicKeyGen $ padSeed seed
 where
  Owner o = owner $ sKey kp
  padSeed s =
    let padLength = max 0 (32 - BS.length s) in BS.replicate padLength 0 <> s
  seed = BSL.toStrict . toLazyByteString . integerDec $ fromIntegral o

vKeyPair :: VKey -> KeyPair
vKeyPair (VKey o) = keyPair o

elaborateVKey :: VKey -> VerificationKey
elaborateVKey = fst . elaborateKeyPair . vKeyPair

elaborateVKeyGenesis :: VKeyGenesis -> VerificationKey
elaborateVKeyGenesis (VKeyGenesis vk) = elaborateVKey vk
