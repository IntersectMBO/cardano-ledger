module Generator.Core.Constants

where

import           Data.Word (Word64)

-- | minimal number of addresses for transaction outputs
minNumGenAddr :: Int
minNumGenAddr = 1

-- | minimal number of addresses for transaction outputs
maxNumGenAddr :: Int
maxNumGenAddr = 2

-- | minimal number of transaction inputs to select
minNumGenInputs :: Int
minNumGenInputs = 1

-- | maximal number of transaction inputs to select
maxNumGenInputs :: Int
maxNumGenInputs = 5

-- | Relative frequency of generated credential registration certificates
frequencyRegKeyCert :: Int
frequencyRegKeyCert = 2

-- | Relative frequency of generated pool registration certificates
frequencyRegPoolCert :: Int
frequencyRegPoolCert = 2

-- | Relative frequency of generated delegation certificates
frequencyDelegationCert :: Int
frequencyDelegationCert = 3

-- | Relative frequency of generated genesis delegation certificates
frequencyGenesisDelegationCert :: Int
frequencyGenesisDelegationCert = 1

-- | Relative frequency of generated credential de-registration certificates
frequencyDeRegKeyCert :: Int
frequencyDeRegKeyCert = 1

-- | Relative frequency of generated pool retirement certificates
frequencyRetirePoolCert :: Int
frequencyRetirePoolCert = 1

-- | Relative frequency of generated MIR certificates
frequencyMIRCert :: Int
frequencyMIRCert = 1

-- | Relative frequency of script credentials in credential registration
-- certificates
frequencyScriptCredReg :: Int
frequencyScriptCredReg = 1

-- | Relative frequency of key credentials in credential registration
-- certificates
frequencyKeyCredReg :: Int
frequencyKeyCredReg = 2

-- | Relative frequency of script credentials in credential de-registration
-- certificates
frequencyScriptCredDeReg :: Int
frequencyScriptCredDeReg = 1

-- | Relative frequency of key credentials in credential de-registration
-- certificates
frequencyKeyCredDeReg :: Int
frequencyKeyCredDeReg = 2

-- | Relative frequency of script credentials in credential delegation
-- certificates
frequencyScriptCredDelegation :: Int
frequencyScriptCredDelegation = 1

-- | Relative frequency of key credentials in credential delegation
-- certificates
frequencyKeyCredDelegation :: Int
frequencyKeyCredDelegation = 2

-- | minimal number of genesis UTxO outputs
minGenesisUTxOouts :: Int
minGenesisUTxOouts = 1

-- | maximal number of genesis UTxO outputs
maxGenesisUTxOouts :: Int
maxGenesisUTxOouts = 5

-- | maximal numbers of generated keypairs
maxNumKeyPairs :: Word64
maxNumKeyPairs = 150

-- | minimal coin value for generated genesis outputs
minGenesisOutputVal :: Integer
minGenesisOutputVal = 1000

-- | maximal coin value for generated genesis outputs
maxGenesisOutputVal :: Integer
maxGenesisOutputVal = 10000

-- | Number of base scripts from which multi sig scripts are built.
numBaseScripts :: Int
numBaseScripts = 5
