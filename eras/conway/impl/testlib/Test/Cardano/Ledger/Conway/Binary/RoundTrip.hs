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
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpecWith)
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary (
  genConwayGovStateWith,
  genDRepPulsingStateWith,
  genEnactStateWith,
  genGovActionState,
  genProposalsWith,
  genPulsingSnapshotWith,
  genRatifyStateWith,
  govActionGeneratorsWith,
 )
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest (..))
import Test.Cardano.Ledger.Core.Binary.RoundTrip

roundTripConwayCommonSpec ::
  forall era.
  (ConwayEraTest era, GovState era ~ ConwayGovState era) => Spec
roundTripConwayCommonSpec = do
  roundTripConwayEraTypesSpec @era
  -- The era-generic state tests take two generators for the `GovState`:
  --
  -- 1. The first is for the roundtrip of `ConwayGovState` itself. It generates the fields that
  --   `ConwayGovState` decodes itself (its current, previous and future `PParams`) with
  --   `genPParamsHalfWithCostModels`, and its nested `Proposals` and `DRepPulsingState`, which
  --   have roundtrip tests of their own, without cost models.
  --
  -- 2. The second is for the `GovState` nested in `UTxOState`, `EpochState` and `NewEpochState`.
  --   `ConwayGovState` has the roundtrip test above, so it is generated without cost models here.
  --
  -- See "Cost models in the governance state roundtrips" below for why.
  roundTripAlonzoCommonSpecWith @era
    ( genConwayGovStateWith
        genPParamsHalfWithCostModels
        genProposalsNoCostModels
        genDRepPulsingStateNoCostModels
    )
    genConwayGovStateNoCostModels

roundTripConwayEraTypesSpec ::
  forall era.
  (Arbitrary (PParams era), Arbitrary (PParamsUpdate era), ConwayEraTest era) =>
  Spec
roundTripConwayEraTypesSpec = do
  describe (eraName @era <> " Transaction Types") $ do
    roundTripEraTypeSpec @era @GovAction
    roundTripEraTypeSpec @era @VotingProcedure
    roundTripEraTypeSpec @era @VotingProcedures
    roundTripEraTypeSpec @era @ProposalProcedure
    roundTripEraTypeSpec @era @Constitution
    prop "CostModels" $ roundTripEraExpectation @era @CostModels
  -- See "Cost models in the governance state roundtrips" below for the choice of generators.
  describe (eraName @era <> " State Types") $ do
    -- `EnactState` decodes its current and previous `PParams` itself.
    roundTripShareEraTypeSpecWith @era $ genEnactStateWith genPParamsHalfWithCostModels
    -- `GovActionState`, `Proposals` and `PulsingSnapshot` use their `Arbitrary` instances: their
    -- own fields are the `GovAction`s, whose `PParamsUpdate`s have cost models half of the time.
    roundTripShareEraTypeSpec @era @GovActionState
    roundTripShareEraTypeSpec @era @Proposals
    proposalsDecodingSpec @era
    -- `DRepPulsingState` has no fields of its own: it only wraps a `PulsingSnapshot` and a
    -- `RatifyState`, which have roundtrip tests of their own, so both are without cost models.
    roundTripShareEraTypeSpecWith @era genDRepPulsingStateNoCostModels
    roundTripShareEraTypeSpec @era @PulsingSnapshot
    -- `RatifyState` decodes its enacted proposals itself, so they come from the `Arbitrary`
    -- instance, while its nested `EnactState` has a roundtrip test of its own.
    roundTripShareEraTypeSpecWith @era $ genRatifyStateWith genEnactStateNoCostModels arbitrary
    roundTripShareEraTypeSpec @era @VState

-- | Like `roundTripShareEraTypeSpec`, but with the given generator
roundTripShareEraTypeSpecWith ::
  forall era t.
  ( Era era
  , Typeable t
  , Show (t era)
  , Eq (t era)
  , Binary.EncCBOR (t era)
  , Binary.DecShareCBOR (t era)
  , Arbitrary (t era)
  , HasCallStack
  ) =>
  Gen (t era) ->
  Spec
roundTripShareEraTypeSpecWith gen =
  prop (show (typeRep $ Proxy @(t era))) $
    forAllShrink gen shrink $
      roundTripShareEraTypeExpectation @era @t

-- Cost models in the governance state roundtrips
-- ==============================================
--
-- Building a `CostModel` is expensive: Plutus builds an evaluation context for it, which takes
-- 16 to 60 ms depending on the language. A roundtrip pays that when it generates a value, and
-- again every time it decodes one, which `roundTripShareEraTypeExpectation` does once per
-- protocol version of the era.
--
-- The governance state types nest each other: `EnactState` is in `RatifyState`, which is in
-- `DRepPulsingState`, which is in `ConwayGovState`, which is in `UTxOState`, `EpochState` and
-- `NewEpochState`. With the `Arbitrary` instances everywhere, every one of these roundtrips
-- decodes all the `PParams` and `PParamsUpdate`s of the types nested in it again, and together
-- they took about 70 minutes of the Conway test suite.
--
-- Cost models are not decoded any differently inside the governance state: every one of these
-- types decodes its `PParams` and `PParamsUpdate`s with their plain `decCBOR`, which the
-- `PParams`, `PParamsUpdate` and `CostModels` roundtrip tests already cover at full strength.
-- So the generators here follow one rule, which keeps every decoder covered:
--
-- 1. The fields that a type decodes itself are generated like its `Arbitrary` instance does,
--    except that its `PParams` have cost models only half of the time
--    (`genPParamsHalfWithCostModels`).
--
-- 2. A nested value whose type has a roundtrip test of its own has no cost models at all. It
--    still has the same random structure as its `Arbitrary` instance generates, which matters,
--    because the nested decoders run with the sharing state of the type that contains them,
--    which their own roundtrip tests do not cover.
--
-- Some other ways to cut the time, and why these generators do not use them:
--
-- 1. No cost models anywhere in the governance state. It is about twice as fast, but then no
--    governance state roundtrip would ever decode a cost model.
--
-- 2. Fewer tests (`modifyMaxSuccess`) for the slow properties. It cuts the coverage of
--    everything in these types, not just of the cost models.
--
-- 3. Cost models only in some of the governance state samples, like 1 in 20. A sample with cost
--    models is so slow that even 1 in 20 adds 40% to the time, and it does not cover anything
--    that the rule above misses.
--
-- 4. Lighter `Arbitrary` instances. Other tests and other projects use them, and they should
--    keep generating the whole range of values.
--
-- 5. A pool of cost models built once and shared by all the generators. It only saves the
--    generation, not the decoding, which is the larger part, and it would make the cost models
--    the same in every test run.
--
-- The generators below reuse the `gen…With` generators that the `Arbitrary` instances are built
-- from, and only swap the generators of nested values. So they generate the same structure as
-- the instances do, and stay that way when the instances change.

-- | `PParams` for the fields that a governance state type decodes itself: half of the time from
-- the `Arbitrary` instance, with cost models, and half of the time without cost models. Each
-- `PParams` is chosen on its own, so a type with two of them gets both kinds together too. That
-- halves the cost models to build and decode, and every run still decodes cost models in each of
-- these types about 50 times. An empty cost models map is also a value that the decoders should
-- handle, and the `Arbitrary` instance rarely generates it.
genPParamsHalfWithCostModels :: ConwayEraTest era => Gen (PParams era)
genPParamsHalfWithCostModels = oneof [arbitrary, genPParamsNoCostModels]

-- The governance state values below have no cost models anywhere, not in their `PParams` and not
-- in the `PParamsUpdate`s of their proposals. They are only for nesting in the roundtrips of
-- other types, see "Cost models in the governance state roundtrips" above.

-- | A `GovActionState` like the `Arbitrary` instance generates, except that a `ParameterChange`
-- has a `PParamsUpdate` without cost models
genGovActionStateNoCostModels :: ConwayEraTest era => Gen (GovActionState era)
genGovActionStateNoCostModels =
  genGovActionState =<< oneof (govActionGeneratorsWith genPParamsUpdateNoCostModels)

-- | `Proposals` like the `Arbitrary` instance generates, with the same range of sizes, except
-- that parameter changes have a `PParamsUpdate` without cost models
genProposalsNoCostModels :: ConwayEraTest era => Gen (Proposals era)
genProposalsNoCostModels = genProposalsWith genPParamsUpdateNoCostModels (0, 30)

-- | An `EnactState` whose current and previous `PParams` have no cost models
genEnactStateNoCostModels :: ConwayEraTest era => Gen (EnactState era)
genEnactStateNoCostModels = genEnactStateWith genPParamsNoCostModels

-- | A `RatifyState` whose `EnactState` and enacted proposals have no cost models
genRatifyStateNoCostModels :: ConwayEraTest era => Gen (RatifyState era)
genRatifyStateNoCostModels =
  genRatifyStateWith genEnactStateNoCostModels genGovActionStateNoCostModels

-- | A `DRepPulsingState` whose `PulsingSnapshot` and `RatifyState` have no cost models
genDRepPulsingStateNoCostModels :: ConwayEraTest era => Gen (DRepPulsingState era)
genDRepPulsingStateNoCostModels =
  genDRepPulsingStateWith
    (genPulsingSnapshotWith genGovActionStateNoCostModels)
    genRatifyStateNoCostModels

-- | A `ConwayGovState` without cost models in its `PParams`, its `Proposals` and its
-- `DRepPulsingState`, for the governance state nested in `UTxOState`, `EpochState` and
-- `NewEpochState`
genConwayGovStateNoCostModels :: ConwayEraTest era => Gen (ConwayGovState era)
genConwayGovStateNoCostModels =
  genConwayGovStateWith
    genPParamsNoCostModels
    genProposalsNoCostModels
    genDRepPulsingStateNoCostModels

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
