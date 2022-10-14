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
import Test.Cardano.Ledger.Binary.RoundTrip
  ( cborTrip,
    roundTripAnnExpectation,
    roundTripCborExpectation,
    roundTripExpectation,
  )
import Test.Cardano.Ledger.Shelley.Generator.Metadata ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

eraRoundTripProps ::
  forall e.
  ( ApplyTx e,
    Arbitrary (Core.TxBody e),
    Arbitrary (Core.TxAuxData e),
    Arbitrary (Core.Value e),
    Arbitrary (Core.Script e),
    Arbitrary (ApplyTxError e),
    ToCBOR (PredicateFailure (Core.EraRule "DELEGS" e)),
    FromCBOR (PredicateFailure (Core.EraRule "DELEGS" e)),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOW" e)),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOW" e))
  ) =>
  TestTree
eraRoundTripProps =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testGroup
        (show v)
        [ testProperty "TxBody" $ roundTripAnnExpectation @(Core.TxBody e) v,
          testProperty "Metadata" $ roundTripAnnExpectation @(Core.TxAuxData e) v,
          testProperty "Value" $ roundTripExpectation @(Core.Value e) v cborTrip,
          testProperty "Script" $ roundTripAnnExpectation @(Core.Script e) v,
          testProperty "ApplyTxError" $ roundTripCborExpectation @(ApplyTxError e) v
        ]
      | v <- [Core.eraProtVerLow @e .. Core.eraProtVerHigh @e]
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ eraRoundTripProps @Shelley,
      eraRoundTripProps @Allegra,
      eraRoundTripProps @Mary
    ]
