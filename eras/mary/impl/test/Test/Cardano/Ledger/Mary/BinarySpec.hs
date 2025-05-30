{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.BinarySpec (spec) where

import Cardano.Ledger.Mary
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as Binary (decoderEquivalenceCoreEraTypesSpec, txSizeSpec)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (RuleListEra (..))
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
import Test.Cardano.Ledger.Mary.TreeDiff ()
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @MaryEra
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @MaryEra
  Binary.txSizeSpec @MaryEra

instance RuleListEra MaryEra where
  type
    EraRules MaryEra =
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
