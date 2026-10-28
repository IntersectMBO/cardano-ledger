{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Binary.RoundTrip (
  roundTripConwayCommonSpec,
  roundTripConwayEraTypesSpec,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import qualified Cardano.Ledger.Binary as Binary
import Cardano.Ledger.Binary.Plain (Tokens (TkBreak, TkListBegin, TkListLen))
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus (CostModels)
import Data.Default (def)
import qualified Data.Foldable as F
import Data.List (isInfixOf)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary (genGovActionState)
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest)
import Test.Cardano.Ledger.Core.Binary.RoundTrip

roundTripConwayCommonSpec :: forall era. ConwayEraTest era => Spec
roundTripConwayCommonSpec = do
  roundTripConwayEraTypesSpec @era
  roundTripAlonzoCommonSpec @era

roundTripConwayEraTypesSpec ::
  forall era.
  ( Arbitrary (PParams era)
  , Arbitrary (PParamsUpdate era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (InstantStake era)
  , EraPParams era
  , EraStake era
  , ConwayEraAccounts era
  ) =>
  Spec
roundTripConwayEraTypesSpec = do
  describe (eraName @era <> " Transaction Types") $ do
    roundTripEraTypeSpec @era @GovAction
    roundTripEraTypeSpec @era @VotingProcedure
    roundTripEraTypeSpec @era @VotingProcedures
    roundTripEraTypeSpec @era @ProposalProcedure
    roundTripEraTypeSpec @era @Constitution
    prop "CostModels" $ roundTripEraExpectation @era @CostModels
  describe (eraName @era <> " State Types") $ do
    roundTripShareEraTypeSpec @era @EnactState
    roundTripShareEraTypeSpec @era @GovActionState
    roundTripShareEraTypeSpec @era @Proposals
    proposalsDecodingSpec @era
    roundTripShareEraTypeSpec @era @DRepPulsingState
    roundTripShareEraTypeSpec @era @PulsingSnapshot
    roundTripShareEraTypeSpec @era @RatifyState
    roundTripShareEraTypeSpec @era @VState

-- Construct CBOR from lists so duplicate IDs survive until decoding.
proposalsDecodingSpec ::
  forall era.
  (EraPParams era, ConwayEraAccounts era, EraStake era) =>
  Spec
proposalsDecodingSpec =
  describe "Proposals decoding" $
    forM_ [eraProtVerLow @era .. eraProtVerHigh @era] $ \version ->
      describe (show version) $
        forM_ arrayForms $ \(arrayName, arrayEncoding) ->
          describe arrayName $
            forM_ (decoders version) $ \(decoderName, decode) ->
              describe decoderName $ do
                forM_
                  [ ("Identical adjacent", \a b -> [a, a, b])
                  , ("Identical separated", \a b -> [a, b, a])
                  , ("Conflicting adjacent", \a b -> [a, a & gasDepositL .~ Coin 2, b])
                  , ("Conflicting separated", \a b -> [a, b, a & gasDepositL .~ Coin 2])
                  ]
                  $ \(name, duplicateActions) ->
                    prop (name <> " duplicate IDs are rejected") $
                      forAll actionGen $ \gas -> do
                        let actions = duplicateActions (withIndex 0 gas) (withIndex 1 gas)
                        err <- expectLeft $ decode $ proposalsEnc version arrayEncoding actions
                        show err
                          `shouldSatisfy` isInfixOf
                            "Final number of elements: 2 does not match the total count that was decoded: 3"
                forM_ [("Empty", []), ("Insertion order", [2, 0, 1])] $ \(name, indices) ->
                  prop (name <> " proposals decode unchanged") $
                    forAll actionGen $ \gas -> do
                      let actions = (\n -> withIndex n gas) <$> indices
                      actual <- expectRight $ decode $ proposalsEnc version arrayEncoding actions
                      F.toList (proposalsActions actual) `shouldBe` actions
  where
    actionGen =
      (gasDepositL .~ Coin 1) <$> resize 3 (genGovActionState @era InfoAction)
    withIndex n gas =
      gas & gasIdL %~ (\gai -> gai {gaidGovActionIx = GovActionIx n})
    arrayForms =
      [ ("Definite", \values -> Em [E $ TkListLen $ fromIntegral $ length values, Em values])
      , ("Indefinite", \values -> Em [E TkListBegin, Em values, E TkBreak])
      ]
    proposalsEnc version arrayEncoding actions =
      Em
        [ E $ TkListLen 2
        , Ev version (def :: GovRelation StrictMaybe)
        , arrayEncoding $ Ev version <$> actions
        ]
    govStateEnc version proposals =
      let state = def :: ConwayGovState era
       in Em
            [ E $ TkListLen 7
            , proposals
            , Ev version $ cgsCommittee state
            , Ev version $ cgsConstitution state
            , Ev version $ cgsCurPParams state
            , Ev version $ cgsPrevPParams state
            , Ev version $ cgsFuturePParams state
            , Ev version $ cgsDRepPulsingState state
            ]
    decoders ::
      Binary.Version ->
      [(String, Enc -> Either Binary.DecoderError (Proposals era))]
    decoders version =
      [ ("Ordinary", Binary.decodeFull version . Plain.serialize)
      ,
        ( "Shared"
        , Binary.decodeFullDecoder version "Proposals regression" Binary.decNoShareCBOR . Plain.serialize
        )
      ,
        ( "Governance state"
        , fmap cgsProposals . Binary.decodeFull version . Plain.serialize . govStateEnc version
        )
      ]
