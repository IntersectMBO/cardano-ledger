{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.BinarySpec (spec) where

import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Babbage
import Cardano.Ledger.Crypto (Crypto)
import Data.Default.Class (def)
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra (..), roundTripEraSpec)

spec :: Spec
spec = do
  specUpgrade @Babbage def
  describe "RoundTrip" $ do
    roundTripAlonzoCommonSpec @Babbage
    -- CostModel serialization changes drastically for Conway, which requires a different
    -- QuickCheck generator, hence Arbitrary can't be reused
    roundTripEraSpec @Babbage @CostModels

instance Crypto c => RuleListEra (BabbageEra c) where
  type
    EraRules (BabbageEra c) =
      '[ "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOW"
       , "UTXOS"
       ]
