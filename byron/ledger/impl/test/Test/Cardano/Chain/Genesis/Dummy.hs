{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Genesis.Dummy
  ( dummyConfig,
    dummyK,
    dummyEpochSlots,
    dummySlotSecurityParam,
    dummyGenesisInitializer,
    dummyGenesisAvvmBalances,
    dummyGeneratedSecrets,
    dummyGenesisSigningKeys,
    dummyGenesisSigningKeysRich,
    dummyGenesisSigningKeysPoor,
    dummyGenesisSecretsRich,
    dummyGenesisSecretsPoor,
    dummyGenesisSpec,
    dummyProtocolParameters,
    dummyGenesisData,
    dummyGenesisHash,
  )
where

import Cardano.Chain.Common
  ( BlockCount (..),
    TxFeePolicy (..),
    TxSizeLinear (..),
    mkKnownLovelace,
    rationalToLovelacePortion,
  )
import Cardano.Chain.Genesis
  ( Config (..),
    FakeAvvmOptions (..),
    GeneratedSecrets (..),
    GenesisAvvmBalances (..),
    GenesisData (..),
    GenesisDelegation (..),
    GenesisHash (..),
    GenesisInitializer (..),
    GenesisSpec (..),
    PoorSecret,
    TestnetBalanceOptions (..),
    generateGenesisConfigWithEntropy,
    gsSigningKeys,
    gsSigningKeysPoor,
  )
import Cardano.Chain.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import Cardano.Chain.Slotting (EpochNumber (..), EpochSlots, SlotCount)
import Cardano.Chain.Update (ProtocolParameters (..), SoftforkRule (..))
import Cardano.Crypto as Crypto (SigningKey, deterministic)
import Cardano.Prelude
import Data.Time (Day (..), UTCTime (..))
import qualified Test.Cardano.Crypto.Dummy as Dummy

dummyConfig :: Config
dummyGeneratedSecrets :: GeneratedSecrets
(dummyConfig, dummyGeneratedSecrets) =
  either (panic . show) identity $
    Crypto.deterministic seed $ -- supply fake entropy to make this pure
      runExceptT $
        generateGenesisConfigWithEntropy startTime dummyGenesisSpec
  where
    seed :: ByteString
    seed = "\0"
    startTime = UTCTime (ModifiedJulianDay 0) 0

dummyK :: BlockCount
dummyK = BlockCount 10

dummyEpochSlots :: EpochSlots
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: SlotCount
dummySlotSecurityParam = kSlotSecurityParam dummyK

dummyGenesisSecretsRich :: [SigningKey]
dummyGenesisSecretsRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSecretsPoor :: [PoorSecret]
dummyGenesisSecretsPoor = gsPoorSecrets dummyGeneratedSecrets

dummyGenesisSigningKeys :: [SigningKey]
dummyGenesisSigningKeys = gsSigningKeys dummyGeneratedSecrets

dummyGenesisSigningKeysRich :: [SigningKey]
dummyGenesisSigningKeysRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSigningKeysPoor :: [SigningKey]
dummyGenesisSigningKeysPoor = gsSigningKeysPoor dummyGeneratedSecrets

dummyGenesisSpec :: GenesisSpec
dummyGenesisSpec =
  UnsafeGenesisSpec
    { gsAvvmDistr = dummyGenesisAvvmBalances,
      gsHeavyDelegation = UnsafeGenesisDelegation mempty,
      gsProtocolParameters = dummyProtocolParameters,
      gsK = dummyK,
      gsProtocolMagic = Dummy.protocolMagic,
      gsInitializer = dummyGenesisInitializer
    }

dummyGenesisAvvmBalances :: GenesisAvvmBalances
dummyGenesisAvvmBalances = GenesisAvvmBalances mempty

dummyProtocolParameters :: ProtocolParameters
dummyProtocolParameters =
  ProtocolParameters
    { ppScriptVersion = 0,
      ppSlotDuration = 7000,
      ppMaxBlockSize = 2000000,
      ppMaxHeaderSize = 2000000,
      ppMaxTxSize = 8192,
      ppMaxProposalSize = 700,
      ppMpcThd = rationalToLovelacePortion 0.01,
      ppHeavyDelThd = rationalToLovelacePortion 0.005,
      ppUpdateVoteThd = rationalToLovelacePortion 0.001,
      ppUpdateProposalThd = rationalToLovelacePortion 0.1,
      ppUpdateProposalTTL = 10,
      ppSoftforkRule =
        SoftforkRule
          { srInitThd = rationalToLovelacePortion 0.9,
            srMinThd = rationalToLovelacePortion 0.6,
            srThdDecrement = rationalToLovelacePortion 0.05
          },
      ppTxFeePolicy =
        TxFeePolicyTxSizeLinear
          (TxSizeLinear (mkKnownLovelace @155381) 43.946),
      ppUnlockStakeEpoch = EpochNumber maxBound
    }

dummyGenesisInitializer :: GenesisInitializer
dummyGenesisInitializer =
  GenesisInitializer
    { giTestBalance =
        TestnetBalanceOptions
          { tboPoors = 12,
            tboRichmen = 4,
            tboTotalBalance = mkKnownLovelace @6000000000000000,
            tboRichmenShare = 0.99 :: Rational
          },
      giFakeAvvmBalance =
        FakeAvvmOptions
          { faoCount = 10,
            faoOneBalance = mkKnownLovelace @100000
          },
      giAvvmBalanceFactor = 1.0 :: Rational,
      giUseHeavyDlg = True
    }

dummyGenesisData :: GenesisData
dummyGenesisData = configGenesisData dummyConfig

dummyGenesisHash :: GenesisHash
dummyGenesisHash = configGenesisHash dummyConfig
