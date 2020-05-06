module Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
    defaultConstants,
  )
where

import Data.Word (Word64)
import Numeric.Natural (Natural)

data Constants = Constants
  { -- | minimal number of transaction inputs to select
    minNumGenInputs :: Int,
    -- | maximal number of transaction inputs to select
    maxNumGenInputs :: Int,
    -- | Relative frequency of generated credential registration certificates
    frequencyRegKeyCert :: Int,
    -- | Relative frequency of generated pool registration certificates
    frequencyRegPoolCert :: Int,
    -- | Relative frequency of generated delegation certificates
    frequencyDelegationCert :: Int,
    -- | Relative frequency of generated genesis delegation certificates
    frequencyGenesisDelegationCert :: Int,
    -- | Relative frequency of generated credential de-registration certificates
    frequencyDeRegKeyCert :: Int,
    -- | Relative frequency of generated pool retirement certificates
    frequencyRetirePoolCert :: Int,
    -- | Relative frequency of generated MIR certificates
    frequencyMIRCert :: Int,
    -- | Relative frequency of script credentials in credential registration
    -- certificates
    frequencyScriptCredReg :: Int,
    -- | Relative frequency of key credentials in credential registration
    -- certificates
    frequencyKeyCredReg :: Int,
    -- | Relative frequency of script credentials in credential de-registration
    -- certificates
    frequencyScriptCredDeReg :: Int,
    -- | Relative frequency of key credentials in credential de-registration
    -- certificates
    frequencyKeyCredDeReg :: Int,
    -- | Relative frequency of script credentials in credential delegation
    -- certificates
    frequencyScriptCredDelegation :: Int,
    -- | Relative frequency of key credentials in credential delegation
    -- certificates
    frequencyKeyCredDelegation :: Int,
    -- | Relative frequency of Prototol/Application Updates in a transaction
    frequencyTxUpdates :: Int,
    -- | minimal number of genesis UTxO outputs
    minGenesisUTxOouts :: Int,
    -- | maximal number of genesis UTxO outputs
    maxGenesisUTxOouts :: Int,
    -- | maximal number of certificates per transaction
    maxCertsPerTx :: Word64,
    -- | maximal number of Txs per block
    maxTxsPerBlock :: Word64,
    -- | maximal numbers of generated keypairs
    maxNumKeyPairs :: Word64,
    -- | minimal coin value for generated genesis outputs
    minGenesisOutputVal :: Integer,
    -- | maximal coin value for generated genesis outputs
    maxGenesisOutputVal :: Integer,
    -- | Number of base scripts from which multi sig scripts are built.
    numBaseScripts :: Int,
    -- | Relative frequency that a transaction does not include any reward withdrawals
    frequencyNoWithdrawals :: Int,
    -- | Relative frequency that a transaction includes a small number of
    -- reward withdrawals, bounded by 'maxAFewWithdrawals'.
    frequencyAFewWithdrawals :: Int,
    -- | Maximum number of reward withdrawals that counts as a small number.
    maxAFewWithdrawals :: Int,
    -- | Relative frequency that a transaction includes any positive number of
    -- reward withdrawals
    frequencyPotentiallyManyWithdrawals :: Int,
    -- | Minimal slot for CHAIN trace generation.
    minSlotTrace :: Int,
    -- | Maximal slot for CHAIN trace generation.
    maxSlotTrace :: Int,
    -- | Lower bound of the MaxEpoch protocol parameter
    frequencyLowMaxEpoch :: Word64,
    maxMinFeeA :: Natural,
    maxMinFeeB :: Natural,
    numCoreNodes :: Word64
  }
  deriving (Show)

defaultConstants :: Constants
defaultConstants =
  Constants
    { minNumGenInputs = 1,
      maxNumGenInputs = 10,
      frequencyRegKeyCert = 2,
      frequencyRegPoolCert = 2,
      frequencyDelegationCert = 3,
      frequencyGenesisDelegationCert = 1,
      frequencyDeRegKeyCert = 1,
      frequencyRetirePoolCert = 1,
      frequencyMIRCert = 1,
      frequencyScriptCredReg = 1,
      frequencyKeyCredReg = 2,
      frequencyScriptCredDeReg = 1,
      frequencyKeyCredDeReg = 2,
      frequencyScriptCredDelegation = 1,
      frequencyKeyCredDelegation = 2,
      frequencyTxUpdates = 10,
      minGenesisUTxOouts = 10,
      maxGenesisUTxOouts = 100,
      maxCertsPerTx = 3,
      maxTxsPerBlock = 10,
      maxNumKeyPairs = 150,
      minGenesisOutputVal = 1000000,
      maxGenesisOutputVal = 100000000,
      numBaseScripts = 3,
      frequencyNoWithdrawals = 75,
      frequencyAFewWithdrawals = 20,
      maxAFewWithdrawals = 10,
      frequencyPotentiallyManyWithdrawals = 5,
      minSlotTrace = 1000,
      maxSlotTrace = 5000,
      frequencyLowMaxEpoch = 6,
      maxMinFeeA = 1000,
      maxMinFeeB = 3,
      numCoreNodes = 7
    }
