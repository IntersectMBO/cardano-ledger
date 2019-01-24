{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Interpreter
  ( interpretDCert
  , tests
  )
where

import Cardano.Prelude

import Data.ByteString.Builder (integerDec, toLazyByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set

import Cardano.Binary.Class (Annotated(..), serialize')
import Cardano.Chain.Delegation as Delegation (Certificate)
import Cardano.Chain.Slotting (EpochIndex)
import Cardano.Crypto.Signing
  ( AProxySecretKey(..)
  , PublicKey
  , SecretKey
  , createPsk
  , deterministicKeyGen
  , noPassSafeSigner
  , pskOmega
  , validateProxySecretKey
  )
import Ledger.Core
  ( Epoch(..)
  , HasOwner(..)
  , KeyPair(..)
  , Owner(..)
  , Slot(..)
  , SlotCount(..)
  , VKey(..)
  , VKeyGenesis(..)
  , keyPair
  )
import Ledger.Delegation (DCert(..), DSEnv(..), dcertGen, delegate, delegator)

import Hedgehog
  (Property, checkSequential, discover, evalEither, forAll, property, withTests)

import Test.Cardano.Crypto.Dummy (dummyProtocolMagicId)


tests :: IO Bool
tests = checkSequential $$discover

prop_interpretedCertsValid :: Property
prop_interpretedCertsValid =
  withTests 50
    . property
    $ do

        -- Generate and interpret a certificate
        cert <- forAll $ interpretDCert <$> dcertGen env

        -- Annotate the omega value for signature checking
        let
          omega = pskOmega cert
          annotatedCert =
            cert { aPskOmega = Annotated omega (serialize' omega) }

        -- Validate the certificate
        evalEither $ validateProxySecretKey dummyProtocolMagicId annotatedCert
 where
  env = DSEnv
    { _dSEnvAllowedDelegators = Set.fromList
      . fmap (VKeyGenesis . VKey . Owner)
      $ [0 .. 6]
    , _dSEnvEpoch             = Epoch 0
    , _dSEnvSlot              = Slot 0
    , _dSEnvLiveness          = SlotCount 20
    }

interpretDCert :: DCert -> Delegation.Certificate
interpretDCert cert = createPsk
  dummyProtocolMagicId
  (noPassSafeSigner delegatorSK)
  delegatePK
  epochIndex
 where
  VKeyGenesis delegatorVKey = delegator cert
  (_         , delegatorSK) = interpretKeyPair $ vKeyPair delegatorVKey
  (delegatePK, _          ) = interpretKeyPair . vKeyPair $ delegate cert

  Epoch e                   = _depoch cert

  epochIndex :: EpochIndex
  epochIndex = fromIntegral e

interpretKeyPair :: KeyPair -> (PublicKey, SecretKey)
interpretKeyPair kp = deterministicKeyGen $ padSeed seed
 where
  Owner o = owner $ sKey kp
  padSeed s =
    let padLength = max 0 (32 - BS.length s) in BS.replicate padLength 0 <> s
  seed = BSL.toStrict . toLazyByteString . integerDec $ fromIntegral o

vKeyPair :: VKey -> KeyPair
vKeyPair (VKey o) = keyPair o
