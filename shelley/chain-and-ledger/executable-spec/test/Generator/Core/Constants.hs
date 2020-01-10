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
