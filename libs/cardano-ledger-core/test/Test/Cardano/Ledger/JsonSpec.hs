{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.JsonSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.PoolParams
import Cardano.Ledger.TxIn
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.JSON

spec :: Spec
spec = do
  describe "RoundTrip JSON" $ do
    roundTripJsonSpec @Coin
    roundTripJsonSpec @ProtVer
    roundTripJsonSpec @Nonce
    roundTripJsonSpec @Url
    roundTripJsonSpec @DnsName
    roundTripJsonSpec @Port
    roundTripJsonSpec @Network
    roundTripJsonSpec @EpochInterval
    roundTripJsonSpec @CostModels
    roundTripJsonSpec @PoolMetadata
    roundTripJsonSpec @StakePoolRelay
    roundTripJsonSpec @(PoolParams StandardCrypto)
    roundTripJsonSpec @(Addr StandardCrypto)
    roundTripJsonSpec @(RewardAccount StandardCrypto)
    roundTripJsonSpec @(Credential 'Witness StandardCrypto)
    roundTripJsonSpec @(DRep StandardCrypto)
    roundTripJsonSpec @(Anchor StandardCrypto)
    roundTripJsonSpec @(TxId StandardCrypto)
