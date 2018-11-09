{-# LANGUAGE NumDecimals #-}

module Test.Cardano.Chain.Genesis.Dummy
  ( dummyConfig
  , dummyConfigStartTime
  , dummyProtocolConstants
  , dummyK
  , dummyEpochSlots
  , dummySlotSecurityParam
  , dummyGenesisInitializer
  , dummyGenesisAvvmBalances
  , dummyGeneratedGenesisData
  , dummyGeneratedSecrets
  , dummyGenesisSecretKeys
  , dummyGenesisSecretKeysRich
  , dummyGenesisSecretKeysPoor
  , dummyGenesisSecretsRich
  , dummyGenesisSecretsPoor
  , dummyGenesisSpec
  , dummyBlockVersionData
  , dummyGenesisData
  , dummyGenesisDataStartTime
  , dummyGenesisHash
  )
where

import Cardano.Prelude

import Cardano.Chain.Genesis
  ( Config(..)
  , FakeAvvmOptions(..)
  , GeneratedGenesisData(..)
  , GeneratedSecrets(..)
  , GenesisAvvmBalances(..)
  , GenesisData(..)
  , GenesisHash(..)
  , GenesisInitializer(..)
  , GenesisSpec(..)
  , PoorSecret
  , RichSecrets(..)
  , TestnetBalanceOptions(..)
  , generateGenesisData
  , genesisProtocolConstantsFromProtocolConstants
  , gsSecretKeys
  , gsSecretKeysPoor
  , gsSecretKeysRich
  , mkConfig
  , noGenesisDelegation
  )
import Cardano.Chain.Update (BlockVersionData(..), SoftforkRule(..))
import Cardano.Core
  ( BlockCount
  , Coeff(..)
  , EpochIndex(..)
  , ProtocolConstants(..)
  , SharedSeed(..)
  , SlotCount
  , Timestamp
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , VssMaxTTL(..)
  , VssMinTTL(..)
  , kEpochSlots
  , kSlotSecurityParam
  , pcBlkSecurityParam
  , unsafeLovelacePortionFromDouble
  )
import Cardano.Crypto (SecretKey)

import Test.Cardano.Crypto.Dummy (dummyProtocolMagic)

dummyConfig :: Config
dummyConfig = dummyConfigStartTime 0

dummyConfigStartTime :: Timestamp -> Config
dummyConfigStartTime = flip mkConfig dummyGenesisSpec

dummyProtocolConstants :: ProtocolConstants
dummyProtocolConstants = ProtocolConstants
  { pcK         = 10
  , pcVssMinTTL = VssMinTTL 2
  , pcVssMaxTTL = VssMaxTTL 6
  }

dummyK :: BlockCount
dummyK = pcBlkSecurityParam dummyProtocolConstants

dummyEpochSlots :: SlotCount
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: SlotCount
dummySlotSecurityParam = kSlotSecurityParam dummyK

dummyGeneratedGenesisData :: GeneratedGenesisData
dummyGeneratedGenesisData = generateGenesisData
  dummyProtocolMagic
  dummyProtocolConstants
  dummyGenesisInitializer
  dummyGenesisAvvmBalances

dummyGeneratedSecrets :: GeneratedSecrets
dummyGeneratedSecrets = ggdSecrets dummyGeneratedGenesisData

dummyGenesisSecretsRich :: [RichSecrets]
dummyGenesisSecretsRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSecretsPoor :: [PoorSecret]
dummyGenesisSecretsPoor = gsPoorSecrets dummyGeneratedSecrets

dummyGenesisSecretKeys :: [SecretKey]
dummyGenesisSecretKeys = gsSecretKeys dummyGeneratedSecrets

dummyGenesisSecretKeysRich :: [SecretKey]
dummyGenesisSecretKeysRich = gsSecretKeysRich dummyGeneratedSecrets

dummyGenesisSecretKeysPoor :: [SecretKey]
dummyGenesisSecretKeysPoor = gsSecretKeysPoor dummyGeneratedSecrets

dummyGenesisSpec :: GenesisSpec
dummyGenesisSpec = UnsafeGenesisSpec
  dummyGenesisAvvmBalances
  dummyFtsSeed
  noGenesisDelegation
  dummyBlockVersionData
  (genesisProtocolConstantsFromProtocolConstants
    dummyProtocolConstants
    dummyProtocolMagic
  )
  dummyGenesisInitializer

dummyGenesisAvvmBalances :: GenesisAvvmBalances
dummyGenesisAvvmBalances = GenesisAvvmBalances mempty

dummyFtsSeed :: SharedSeed
dummyFtsSeed = SharedSeed "c2tvdm9yb2RhIEdndXJkYSBib3JvZGEgcHJvdm9kYSA="

dummyBlockVersionData :: BlockVersionData
dummyBlockVersionData = BlockVersionData
  0
  7000
  2000000
  2000000
  4096
  700
  (unsafeLovelacePortionFromDouble 0.01)
  (unsafeLovelacePortionFromDouble 0.005)
  (unsafeLovelacePortionFromDouble 0.001)
  (unsafeLovelacePortionFromDouble 0.1)
  10
  (SoftforkRule
    (unsafeLovelacePortionFromDouble 0.9)
    (unsafeLovelacePortionFromDouble 0.6)
    (unsafeLovelacePortionFromDouble 0.05)
  )
  (TxFeePolicyTxSizeLinear $ TxSizeLinear (Coeff 155381) (Coeff 43.946))
  (EpochIndex maxBound)

dummyGenesisInitializer :: GenesisInitializer
dummyGenesisInitializer = GenesisInitializer
  (TestnetBalanceOptions 12 4 6e17 0.99 True)
  (FakeAvvmOptions 10 100000)
  (unsafeLovelacePortionFromDouble 1)
  True
  0

dummyGenesisData :: GenesisData
dummyGenesisData = configGenesisData dummyConfig

dummyGenesisDataStartTime :: Timestamp -> GenesisData
dummyGenesisDataStartTime = configGenesisData . dummyConfigStartTime

dummyGenesisHash :: GenesisHash
dummyGenesisHash = configGenesisHash dummyConfig
