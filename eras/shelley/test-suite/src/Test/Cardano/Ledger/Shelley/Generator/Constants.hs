module Test.Cardano.Ledger.Shelley.Generator.Constants
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
    -- | Relative frequency of Metadata in a transaction
    frequencyTxWithMetadata :: Int,
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
    -- | Number of simple scripts which appear in the choices, the remainder are compound (MofN, All, Any, etc.) scripts
    numSimpleScripts :: Int,
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
    numCoreNodes :: Word64,
    minTreasury :: Integer,
    maxTreasury :: Integer,
    minReserves :: Integer,
    maxReserves :: Integer,
    -- | When generating Tx, we want the UTxO size to fluctuate around this point. If
    --   it gets too small, we can't balance the fee, too large it gets too complicated.
    genTxStableUtxoSize :: Int,
    -- | If we need to grow the Utxo when generating a Tx, how much should it grow by.
    genTxUtxoIncrement :: Int
  }
  deriving (Show)

defaultConstants :: Constants
defaultConstants =
  Constants
    { minNumGenInputs = 1,
      maxNumGenInputs = 5,
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
      frequencyTxWithMetadata = 10,
      minGenesisUTxOouts = 10,
      maxGenesisUTxOouts = 100,
      maxCertsPerTx = 3,
      maxTxsPerBlock = 10,
      maxNumKeyPairs = 150,
      minGenesisOutputVal = 1000000,
      maxGenesisOutputVal = 100000000,
      numBaseScripts = 3,
      numSimpleScripts = 20,
      frequencyNoWithdrawals = 75,
      frequencyAFewWithdrawals = 20,
      maxAFewWithdrawals = 10,
      frequencyPotentiallyManyWithdrawals = 5,
      minSlotTrace = 1000,
      maxSlotTrace = 5000,
      frequencyLowMaxEpoch = 200,
      maxMinFeeA = 1000,
      maxMinFeeB = 3,
      numCoreNodes = 7,
      minTreasury = 1000000,
      maxTreasury = 10000000,
      minReserves = 1000000,
      maxReserves = 10000000,
      genTxStableUtxoSize = 100,
      genTxUtxoIncrement = 3
    }
