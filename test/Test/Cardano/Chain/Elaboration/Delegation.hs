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
import Hedgehog
  (Group, checkSequential, evalEither, forAll, property)

import Cardano.Binary (Annotated(..), serialize')
import Cardano.Chain.Common (mkStakeholderId)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation as Concrete
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Slotting (EpochIndex)
import Cardano.Crypto (ProtocolMagicId)
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

import Test.Cardano.Chain.Config (readMainetCfg)
import Test.Cardano.Chain.Elaboration.Keys
  (elaborateKeyPair, elaborateVKeyGenesis, vKeyPair)
import Test.Options (TestScenario, TSProperty, withTestsTS)

tests :: TestScenario -> IO Bool
tests ts = checkSequential (($$discoverPropArg :: TestScenario -> Group) ts)


ts_prop_elaboratedCertsValid :: TSProperty
ts_prop_elaboratedCertsValid =
  withTestsTS 50
    . property
    $ do
        config <- readMainetCfg

        let pm = Genesis.configProtocolMagicId config

        -- Generate and elaborate a certificate
        cert   <- forAll $ elaborateDCertAnnotated pm <$> dcertGen env

        -- Validate the certificate
        evalEither $ validateProxyVerificationKey pm cert
 where
  env = DSEnv
    { _dSEnvAllowedDelegators = Set.fromList
      . fmap (VKeyGenesis . VKey . Owner)
      $ [0 .. 6]
    , _dSEnvEpoch    = Epoch 0
    , _dSEnvSlot     = Slot 0
    , _dSEnvLiveness = SlotCount 20
    }


elaborateDCert :: ProtocolMagicId -> DCert -> Delegation.Certificate
elaborateDCert pm cert = createPsk
  pm
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


elaborateDCertAnnotated :: ProtocolMagicId -> DCert -> Delegation.ACertificate ByteString
elaborateDCertAnnotated pm = annotateDCert . elaborateDCert pm
 where
  annotateDCert
    :: Delegation.Certificate
    -> Delegation.ACertificate ByteString
  annotateDCert cert = cert { aPskOmega = Annotated omega (serialize' omega) }
    where omega = pskOmega cert


elaborateDSEnv :: DSEnv -> Concrete.SchedulingEnvironment
elaborateDSEnv abstractEnv = Concrete.SchedulingEnvironment
  { Concrete.seGenesisKeys  = Set.fromList $
      mkStakeholderId . elaborateVKeyGenesis <$> Set.toList genesisKeys
  , Concrete.seCurrentEpoch = fromIntegral e
  , Concrete.seCurrentSlot  = s
  , Concrete.seLiveness     = fromIntegral d
  }
 where
  DSEnv genesisKeys (Epoch e) (Slot s) (SlotCount d) = abstractEnv
