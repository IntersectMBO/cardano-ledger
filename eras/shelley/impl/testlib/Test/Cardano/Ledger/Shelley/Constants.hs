{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Constants (
  Constants (..),
  defaultConstants,
)
where

import Cardano.Ledger.BaseTypes (Version, natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Data.Word (Word64)

data Constants = Constants
  { minNumGenInputs :: Int
  -- ^ minimal number of transaction inputs to select
  , maxNumGenInputs :: Int
  -- ^ maximal number of transaction inputs to select
  , frequencyRegCert :: Int
  -- ^ Relative frequency of generated credential registration certificates
  , frequencyRegPoolCert :: Int
  -- ^ Relative frequency of generated pool registration certificates
  , frequencyDelegCert :: Int
  -- ^ Relative frequency of generated delegation certificates
  , frequencyGenesisDelegationCert :: Int
  -- ^ Relative frequency of generated genesis delegation certificates
  , frequencyDeRegKeyCert :: Int
  -- ^ Relative frequency of generated credential de-registration certificates
  , frequencyRetirePoolCert :: Int
  -- ^ Relative frequency of generated pool retirement certificates
  , frequencyMIRCert :: Int
  -- ^ Relative frequency of generated MIR certificates
  , frequencyScriptCredReg :: Int
  -- ^ Relative frequency of script credentials in credential registration
  -- certificates
  , frequencyKeyCredReg :: Int
  -- ^ Relative frequency of key credentials in credential registration
  -- certificates
  , frequencyScriptCredDeReg :: Int
  -- ^ Relative frequency of script credentials in credential de-registration
  -- certificates
  , frequencyKeyCredDeReg :: Int
  -- ^ Relative frequency of key credentials in credential de-registration
  -- certificates
  , frequencyScriptCredDelegation :: Int
  -- ^ Relative frequency of script credentials in credential delegation
  -- certificates
  , frequencyKeyCredDelegation :: Int
  -- ^ Relative frequency of key credentials in credential delegation
  -- certificates
  , frequencyTxUpdates :: Int
  -- ^ Relative frequency of Prototol/Application Updates in a transaction
  , frequencyTxWithMetadata :: Int
  -- ^ Relative frequency of Metadata in a transaction
  , minGenesisUTxOouts :: Int
  -- ^ minimal number of genesis UTxO outputs
  , maxGenesisUTxOouts :: Int
  -- ^ maximal number of genesis UTxO outputs
  , maxCertsPerTx :: Word64
  -- ^ maximal number of certificates per transaction
  , maxTxsPerBlock :: Word64
  -- ^ maximal number of Txs per block
  , maxNumKeyPairs :: Word64
  -- ^ maximal numbers of generated keypairs
  , minGenesisOutputVal :: Integer
  -- ^ minimal coin value for generated genesis outputs
  , maxGenesisOutputVal :: Integer
  -- ^ maximal coin value for generated genesis outputs
  , numBaseScripts :: Int
  -- ^ Number of base scripts from which multi sig scripts are built.
  , numSimpleScripts :: Int
  -- ^ Number of simple scripts which appear in the choices, the remainder are compound (MofN, All, Any, etc.) scripts
  , frequencyNoWithdrawals :: Int
  -- ^ Relative frequency that a transaction does not include any reward withdrawals
  , frequencyAFewWithdrawals :: Int
  -- ^ Relative frequency that a transaction includes a small number of
  -- reward withdrawals, bounded by 'maxAFewWithdrawals'.
  , maxAFewWithdrawals :: Int
  -- ^ Maximum number of reward withdrawals that counts as a small number.
  , frequencyPotentiallyManyWithdrawals :: Int
  -- ^ Relative frequency that a transaction includes any positive number of
  -- reward withdrawals
  , minSlotTrace :: Int
  -- ^ Minimal slot for CHAIN trace generation.
  , maxSlotTrace :: Int
  -- ^ Maximal slot for CHAIN trace generation.
  , frequencyLowMaxEpoch :: Word64
  -- ^ Lower bound of the MaxEpoch protocol parameter
  , maxMinFeeA :: Coin
  , maxMinFeeB :: Coin
  , numCoreNodes :: Word64
  , minTreasury :: Integer
  , maxTreasury :: Integer
  , minReserves :: Integer
  , maxReserves :: Integer
  , minMajorPV :: Version
  , maxMajorPV :: Version
  , genTxStableUtxoSize :: Int
  -- ^ When generating Tx, we want the UTxO size to fluctuate around this point. If
  --   it gets too small, we can't balance the fee, too large it gets too complicated.
  , genTxUtxoIncrement :: Int
  -- ^ If we need to grow the Utxo when generating a Tx, how much should it grow by.
  }
  deriving (Show)

defaultConstants :: Constants
defaultConstants =
  Constants
    { minNumGenInputs = 1
    , maxNumGenInputs = 5
    , frequencyRegCert = 2
    , frequencyRegPoolCert = 2
    , frequencyDelegCert = 3
    , frequencyGenesisDelegationCert = 1
    , frequencyDeRegKeyCert = 1
    , frequencyRetirePoolCert = 1
    , frequencyMIRCert = 1
    , frequencyScriptCredReg = 1
    , frequencyKeyCredReg = 2
    , frequencyScriptCredDeReg = 1
    , frequencyKeyCredDeReg = 2
    , frequencyScriptCredDelegation = 1
    , frequencyKeyCredDelegation = 2
    , frequencyTxUpdates = 10
    , frequencyTxWithMetadata = 10
    , minGenesisUTxOouts = 10
    , maxGenesisUTxOouts = 100
    , maxCertsPerTx = 3
    , maxTxsPerBlock = 10
    , maxNumKeyPairs = 150
    , minGenesisOutputVal = 1000000
    , maxGenesisOutputVal = 100000000
    , numBaseScripts = 3
    , numSimpleScripts = 20
    , frequencyNoWithdrawals = 75
    , frequencyAFewWithdrawals = 20
    , maxAFewWithdrawals = 10
    , frequencyPotentiallyManyWithdrawals = 5
    , minSlotTrace = 1000
    , maxSlotTrace = 5000
    , frequencyLowMaxEpoch = 200
    , maxMinFeeA = Coin 2
    , maxMinFeeB = Coin 2
    , numCoreNodes = 7
    , minTreasury = 1000000
    , maxTreasury = 10000000
    , minReserves = 1000000
    , maxReserves = 10000000
    , minMajorPV = natVersion @2
    , maxMajorPV = maxBound
    , genTxStableUtxoSize = 100
    , genTxUtxoIncrement = 3
    }
