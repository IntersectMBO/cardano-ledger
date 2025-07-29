{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.TranslationSpec (spec) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.BaseTypes hiding ((==>))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.PParams
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.QuickCheck.Monadic (monadicIO, run)

spec :: Spec
spec = describe "Translation" $ do
  describe "encoded mary types can be decoded as alonzo types" $ do
    prop "decoding auxilliary (Annotator)" $
      embedTripAnnExpectation @(TxAuxData MaryEra) @(TxAuxData AlonzoEra)
        (eraProtVerLow @MaryEra)
        (eraProtVerLow @AlonzoEra)
        (\_ _ -> pure ())
    prop "decoding auxilliary" $
      embedTripExpectation @(TxAuxData MaryEra) @(TxAuxData AlonzoEra)
        (eraProtVerLow @MaryEra)
        (eraProtVerLow @AlonzoEra)
        cborTrip
        (\_ _ -> pure ())
    prop "decoding txbody" $ \txBody ->
      let hasDeprecatedField =
            case txBody ^. updateTxBodyL of
              SNothing -> False
              SJust (Update (ProposedPPUpdates ups) _) ->
                any (\ppu -> isSJust (ppu ^. ppuMinUTxOValueL)) ups
       in not hasDeprecatedField
            ==> monadicIO
            $ run
            $ do
              embedTripAnnExpectation @(TxBody MaryEra) @(TxBody AlonzoEra)
                (eraProtVerLow @MaryEra)
                (eraProtVerLow @AlonzoEra)
                (\_ _ -> pure ())
                txBody
              embedTripExpectation @(TxBody MaryEra) @(TxBody AlonzoEra)
                (eraProtVerLow @MaryEra)
                (eraProtVerLow @AlonzoEra)
                cborTrip
                (\_ _ -> pure ())
                txBody
    prop "decoding witnesses (Annotator)" $
      embedTripAnnExpectation @(TxWits MaryEra) @(TxWits AlonzoEra)
        (eraProtVerLow @MaryEra)
        (eraProtVerLow @AlonzoEra)
        (\_ _ -> pure ())
    prop "decoding witnesses" $
      embedTripExpectation @(TxWits MaryEra) @(TxWits AlonzoEra)
        (eraProtVerLow @MaryEra)
        (eraProtVerLow @AlonzoEra)
        cborTrip
        (\_ _ -> pure ())
