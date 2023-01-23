{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (ApplyTx, ApplyTxError)
import Control.State.Transition.Extended (PredicateFailure)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Binary.RoundTrip (
  cborTrip,
  roundTripAnnExpectation,
  roundTripCborExpectation,
  roundTripExpectation,
 )
import Test.Cardano.Ledger.Shelley.Generator.TxAuxData ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

eraRoundTripProps ::
  forall e.
  ( ApplyTx e
  , Arbitrary (Core.TxBody e)
  , Arbitrary (Core.TxAuxData e)
  , Arbitrary (Core.Value e)
  , Arbitrary (Core.Script e)
  , Arbitrary (ApplyTxError e)
  , ToCBOR (PredicateFailure (Core.EraRule "LEDGER" e))
  , FromCBOR (PredicateFailure (Core.EraRule "LEDGER" e))
  ) =>
  TestTree
eraRoundTripProps =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "TxBody" $ roundTripAnnExpectation @(Core.TxBody e)
    , testProperty "Metadata" $ roundTripAnnExpectation @(Core.TxAuxData e)
    , testProperty "Value" $ roundTripExpectation @(Core.Value e) cborTrip
    , testProperty "Script" $ roundTripAnnExpectation @(Core.Script e)
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
