{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.BinarySpec (spec) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Crypto (Crypto)
import Data.Default.Class (def)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra (..))
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

spec :: Spec
spec = do
  specUpgrade @Allegra def
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @Allegra

instance Crypto c => RuleListEra (AllegraEra c) where
  type
    EraRules (AllegraEra c) =
      '[ "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOW"
       ]
