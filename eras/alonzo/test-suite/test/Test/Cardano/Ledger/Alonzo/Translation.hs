{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Translation (
  tests,
)
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.BaseTypes hiding ((==>))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.PParams
import Lens.Micro
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck ((==>))
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Translation"
    [ alonzoEncodeDecodeTests
    ]

alonzoEncodeDecodeTests :: TestTree
alonzoEncodeDecodeTests =
  testGroup
    "encoded mary types can be decoded as alonzo types"
    [ testProperty "decoding auxilliary" $
        embedTripAnnExpectation @(TxAuxData MaryEra) @(TxAuxData AlonzoEra)
          (eraProtVerLow @MaryEra)
          (eraProtVerLow @AlonzoEra)
          (\_ _ -> pure ())
    , testProperty "decoding txbody" $ \txBody ->
        let hasDeprecatedField =
              case txBody ^. updateTxBodyL of
                SNothing -> False
                SJust (Update (ProposedPPUpdates ups) _) ->
                  any (\ppu -> isSJust (ppu ^. ppuMinUTxOValueL)) ups
         in not hasDeprecatedField ==>
              monadicIO
                ( run $
                    embedTripAnnExpectation @(TxBody MaryEra) @(TxBody AlonzoEra)
                      (eraProtVerLow @MaryEra)
                      (eraProtVerLow @AlonzoEra)
                      (\_ _ -> pure ())
                      txBody
                )
    , testProperty "decoding witnesses" $
        embedTripAnnExpectation @(TxWits MaryEra) @(TxWits AlonzoEra)
          (eraProtVerLow @MaryEra)
          (eraProtVerLow @AlonzoEra)
          (\_ _ -> pure ())
    ]
