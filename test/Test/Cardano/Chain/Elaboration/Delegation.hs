{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Elaboration.Delegation
  ( elaborateDCert
  , tests
  )
where

import Cardano.Prelude

import qualified Data.Set as Set
import Hedgehog
  (Property, checkSequential, discover, evalEither, forAll, property, withTests)

import Cardano.Binary.Class (Annotated(..), serialize')
import Cardano.Chain.Delegation as Delegation (Certificate)
import Cardano.Chain.Slotting (EpochIndex)
import Cardano.Crypto.Signing
  ( AProxyVerificationKey(..)
  , createPsk
  , noPassSafeSigner
  , pskOmega
  , validateProxyVerificationKey
  )
import Ledger.Core
  (Epoch(..), Owner(..), Slot(..), SlotCount(..), VKey(..), VKeyGenesis(..))
import Ledger.Delegation (DCert(..), DSEnv(..), dcertGen, delegate, delegator)

import qualified Cardano.Chain.Genesis as Genesis

import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Cardano.Chain.Elaboration.Keys (elaborateKeyPair, vKeyPair)

tests :: IO Bool
tests = checkSequential $$discover

prop_elaboratedCertsValid :: Property
prop_elaboratedCertsValid =
  withTests 50
    . property
    $ do
        config <- readMainetCfg

        -- Generate and elaborate a certificate
        cert   <- forAll $ elaborateDCert config <$> dcertGen env

        -- Annotate the omega value for signature checking
        let
          omega = pskOmega cert

          annotatedCert =
            cert { aPskOmega = Annotated omega (serialize' omega) }

          pm = Genesis.configProtocolMagicId config

        -- Validate the certificate
        evalEither $ validateProxyVerificationKey pm annotatedCert
 where
  env = DSEnv
    { _dSEnvAllowedDelegators = Set.fromList
      . fmap (VKeyGenesis . VKey . Owner)
      $ [0 .. 6]
    , _dSEnvEpoch    = Epoch 0
    , _dSEnvSlot     = Slot 0
    , _dSEnvLiveness = SlotCount 20
    }

elaborateDCert :: Genesis.Config -> DCert -> Delegation.Certificate
elaborateDCert config cert = createPsk
  (Genesis.configProtocolMagicId config)
  (noPassSafeSigner delegatorSK)
  delegatePK
  epochIndex
 where
  VKeyGenesis delegatorVKey = delegator cert
  (_         , delegatorSK) = elaborateKeyPair $ vKeyPair delegatorVKey
  (delegatePK, _          ) = elaborateKeyPair . vKeyPair $ delegate cert

  Epoch e = _depoch cert

  epochIndex :: EpochIndex
  epochIndex = fromIntegral e
