{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API (ApplyTx, ApplyTxError)
import Control.State.Transition.Extended (PredicateFailure)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripCborExpectation,
 )
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Generator.TxAuxData ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

eraRoundTripProps ::
  forall e.
  ( ApplyTx e
  , Arbitrary (ApplyTxError e)
  , EncCBOR (PredicateFailure (EraRule "LEDGER" e))
  , DecCBOR (PredicateFailure (EraRule "LEDGER" e))
  ) =>
  TestTree
eraRoundTripProps =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "ApplyTxError" $ roundTripCborExpectation @(ApplyTxError e)
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ eraRoundTripProps @ShelleyEra
    , eraRoundTripProps @AllegraEra
    , eraRoundTripProps @MaryEra
    ]
