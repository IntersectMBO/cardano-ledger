{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.BinarySpec (spec) where

import Cardano.Ledger.Allegra
import Data.Default (def)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (specUpgrade)
import Test.Cardano.Ledger.Core.Binary as Binary (decoderEquivalenceCoreEraTypesSpec, txSizeSpec)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra (..))
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

spec :: Spec
spec = do
  specUpgrade @AllegraEra def
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @AllegraEra
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @AllegraEra
  Binary.txSizeSpec @AllegraEra

instance RuleListEra AllegraEra where
  type
    EraRules AllegraEra =
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
