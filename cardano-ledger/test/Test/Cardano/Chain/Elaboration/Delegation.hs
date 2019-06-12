{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Elaboration.Delegation
  ( elaborateDCert
  , elaborateDCertAnnotated
  , elaborateDSEnv
  , tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Data.Set as Set
import Hedgehog (assert, forAll, property)

import Cardano.Binary (Annotated(..), serialize')
import Cardano.Chain.Common (hashKey)
import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.Delegation as Concrete
import qualified Cardano.Chain.Delegation as Concrete.Certificate
import qualified Cardano.Chain.Delegation.Validation.Scheduling as Scheduling
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as Concrete
import Cardano.Crypto (ProtocolMagicId)
import Cardano.Crypto.Signing (noPassSafeSigner)
import Ledger.Core
  ( BlockCount(..)
  , Epoch(..)
  , Owner(..)
  , Slot(..)
  , VKey(..)
  , VKeyGenesis(..)
  )
import Ledger.Delegation (DCert(..), DSEnv(..), dcertGen, delegate, delegator)

import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Cardano.Chain.Elaboration.Keys
  (elaborateKeyPair, elaborateVKeyGenesis, vKeyPair)
import qualified Test.Cardano.Crypto.Dummy as Dummy
import Test.Options (TSGroup, TSProperty, withTestsTS)


tests :: TSGroup
tests = $$discoverPropArg


ts_prop_elaboratedCertsValid :: TSProperty
ts_prop_elaboratedCertsValid =
  withTestsTS 50
    . property
    $ do
        config <- readMainetCfg

        let pm = Genesis.configProtocolMagicId config

        -- Generate and elaborate a certificate
        cert <- forAll $ elaborateDCertAnnotated pm <$> dcertGen env

        -- Validate the certificate
        assert
          $ Concrete.Certificate.isValid (Annotated pm (serialize' pm)) cert
 where
  env = DSEnv
    { _dSEnvAllowedDelegators = Set.fromList
      . fmap (VKeyGenesis . VKey . Owner)
      $ [0 .. 6]
    , _dSEnvEpoch       = Epoch 0
    , _dSEnvSlot        = Slot 0
    , _dSEnvK           = BlockCount 2160
    }


elaborateDCert :: ProtocolMagicId -> DCert -> Concrete.Certificate
elaborateDCert pm cert = Concrete.mkCertificate
  pm
  (noPassSafeSigner delegatorSK)
  delegateVK
  epochNo
 where
  VKeyGenesis delegatorVKey = delegator cert
  (_         , delegatorSK) = elaborateKeyPair $ vKeyPair delegatorVKey
  (delegateVK, _          ) = elaborateKeyPair . vKeyPair $ delegate cert

  Epoch e = _depoch cert

  epochNo :: Concrete.EpochNumber
  epochNo = fromIntegral e


elaborateDCertAnnotated
  :: ProtocolMagicId -> DCert -> Concrete.ACertificate ByteString
elaborateDCertAnnotated pm = annotateDCert . elaborateDCert pm
 where
  annotateDCert :: Concrete.Certificate -> Concrete.ACertificate ByteString
  annotateDCert cert = cert
    { Concrete.Certificate.aEpoch = Annotated omega (serialize' omega)
    }
    where omega = Concrete.Certificate.epoch cert


elaborateDSEnv :: DSEnv -> Scheduling.Environment
elaborateDSEnv abstractEnv = Scheduling.Environment
  { Scheduling.protocolMagic = Dummy.annotatedProtocolMagicId
  , Scheduling.allowedDelegators = Set.fromList
    $   hashKey
    .   elaborateVKeyGenesis
    <$> Set.toList genesisKeys
  , Scheduling.currentEpoch = fromIntegral e
  , Scheduling.currentSlot = Concrete.SlotNumber s
  , Scheduling.k           = Concrete.BlockCount k
  }
  where
    DSEnv genesisKeys (Epoch e) (Slot s) (BlockCount k) = abstractEnv
