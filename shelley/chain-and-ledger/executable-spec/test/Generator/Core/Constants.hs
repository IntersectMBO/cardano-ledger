module Generator.Core.Constants

where

-- | minimal number of addresses for transaction outputs
minNumGenAddr :: Int
minNumGenAddr = 1

-- | minimal number of addresses for transaction outputs
maxNumGenAddr :: Int
maxNumGenAddr = 3

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
