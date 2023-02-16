{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (ApplyTx, ApplyTxError)
import Control.State.Transition.Extended (PredicateFailure)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripAnnRangeExpectation,
  roundTripCborExpectation,
 )
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Protocol.TPraos.TxAuxData ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

eraRoundTripProps ::
  forall e.
  ( ApplyTx e
  , Arbitrary (TxBody e)
  , Arbitrary (TxAuxData e)
  , Arbitrary (Value e)
  , Arbitrary (Script e)
  , Arbitrary (ApplyTxError e)
  , EncCBOR (PredicateFailure (EraRule "LEDGER" e))
  , DecCBOR (PredicateFailure (EraRule "LEDGER" e))
  ) =>
  TestTree
eraRoundTripProps =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "TxBody" $
        roundTripAnnRangeExpectation @(TxBody e)
          (eraProtVerLow @e)
          (eraProtVerHigh @e)
    , testProperty "Metadata" $
        roundTripAnnRangeExpectation @(TxAuxData e)
          (eraProtVerLow @e)
          (eraProtVerHigh @e)
    , testProperty "Value" $ roundTripCborExpectation @(Value e)
    , testProperty "Script" $
        roundTripAnnRangeExpectation @(Script e)
          (eraProtVerLow @e)
          (eraProtVerHigh @e)
    , testProperty "ApplyTxError" $ roundTripCborExpectation @(ApplyTxError e)
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ eraRoundTripProps @Shelley
    , eraRoundTripProps @Allegra
    , eraRoundTripProps @Mary
    ]
