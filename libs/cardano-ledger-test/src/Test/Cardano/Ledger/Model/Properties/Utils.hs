{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.Properties.Utils where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError (..))
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Control.State.Transition.Extended
import Data.List (nub, (\\))
import Test.Cardano.Ledger.Model.API
import Test.Cardano.Ledger.Model.Elaborators
  ( ApplyBlockTransitionError (..),
    ElaborateApplyTxError (..),
    ElaborateBlockError (..),
    ElaborateEraModel (..),
    EraElaboratorState (..),
    TestCredentialInfo (..),
    elaborateBlocks_,
    eraFeatureSet,
  )
import Test.Cardano.Ledger.Model.FeatureSet
import Test.Cardano.Ledger.Model.Rules (ModelPredicateFailure (..))
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.Tasty.QuickCheck

-- | apply a list of ModelEpoch to an empty ledger and return the resulting
-- state, or the error if one occured
chainModelInteractionWith ::
  forall era proxy.
  ElaborateEraModel era =>
  proxy era ->
  ModelGenesis (EraFeatureSet era) ->
  [ModelEpoch (EraFeatureSet era)] ->
  (Either (ElaborateBlockError era) (), (NewEpochState era, EraElaboratorState era))
chainModelInteractionWith _ gen modelBlocks =
  let -- TODO, pass this in as a generator.
      globals = testGlobals
      elabState = elaborateInitialState globals gen
   in elaborateBlocks_ globals modelBlocks elabState

-- | Apply a list of ModelEpoch's to an empty ledger, then check the resulting
-- ledger against a user supplied predicate.
testChainModelInteractionWith ::
  ( Testable prop,
    ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era
  ) =>
  proxy era ->
  (NewEpochState era -> EraElaboratorState era -> prop) ->
  ModelGenesis (EraFeatureSet era) ->
  [ModelEpoch AllModelFeatures] ->
  Property
testChainModelInteractionWith proxy p a = filterChainModelProp proxy $ \b ->
  let (result, (nes, ees)) = chainModelInteractionWith proxy a b
   in case result of
        Right () -> property $! p nes ees
        Left bad -> counterexample (show bad) $ False

testChainModelInteractionWith' ::
  ( Testable prop,
    ElaborateEraModel era,
    Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (Core.Tx era),
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era
  ) =>
  proxy era ->
  (NewEpochState era -> EraElaboratorState era -> prop) ->
  ModelGenesis (EraFeatureSet era) ->
  [ModelEpoch (EraFeatureSet era)] ->
  Property
testChainModelInteractionWith' proxy p a b =
  let (result, (nes, ees)) = chainModelInteractionWith proxy a b
   in case result of
        Right () -> property $! p nes ees
        Left bad ->
          counterexample (show $ fmap _tciKey $ _eesKeys ees) $
            counterexample (show bad) $
              False

compareLists :: forall a. (Show a, Eq a) => [a] -> [a] -> Property
compareLists a b = case nub a \\ nub b of
  [] -> property True
  _ -> a === b

-- | Apply a list of ModelEpoch's to an empty ledger, then check the resulting
-- error is the one predicted by the model.
testChainModelInteractionRejection ::
  forall era proxy.
  ( ElaborateEraModel era,
    Eq (PredicateFailure (Core.EraRule "LEDGER" era)),
    Show (PredicateFailure (Core.EraRule "LEDGER" era))
  ) =>
  proxy era ->
  ModelPredicateFailure (EraFeatureSet era) ->
  ModelGenesis (EraFeatureSet era) ->
  [ModelEpoch AllModelFeatures] ->
  Property
testChainModelInteractionRejection proxy e a = filterChainModelProp proxy $ \b ->
  let (result, (nes, ees)) = chainModelInteractionWith proxy a b
   in case result of
        Left e' ->
          let elaboratedError = toEraPredicateFailure @era e (nes, ees)
           in case (e', elaboratedError) of
                (ElaborateBlockError_ApplyTx (ElaborateApplyTxError {_eateErr = ApplyTxError te}), ApplyBlockTransitionError_Tx (ApplyTxError te')) ->
                  compareLists te te'
        -- fallthrough if/when more error types are added
        -- (te, te') -> te === te'

        Right _ -> counterexample "no error encountered" False

-- | Apply a list of ModelEpoch's to an empty ledger, and fail if there was an
-- error
testChainModelInteraction ::
  ( Show (PredicateFailure (Core.EraRule "LEDGER" era)),
    ElaborateEraModel era,
    Show (Core.Tx era),
    Show (Core.Script era),
    LedgerState.TransUTxOState Show era
  ) =>
  proxy era ->
  ModelGenesis (EraFeatureSet era) ->
  [ModelEpoch AllModelFeatures] ->
  Property
testChainModelInteraction proxy = testChainModelInteractionWith proxy $ (\x y -> x `seq` y `seq` True)

type AllScriptFeatures = ('TyScriptFeature 'True 'True)

type AllModelFeatures = 'FeatureSet 'ExpectAnyOutput AllScriptFeatures

filterChainModelProp ::
  forall era proxy.
  (ElaborateEraModel era) =>
  proxy era ->
  ([ModelEpoch (EraFeatureSet era)] -> Property) ->
  [ModelEpoch AllModelFeatures] ->
  Property
filterChainModelProp proxy f xs =
  case traverse (filterFeatures (eraFeatureSet proxy)) xs of
    Nothing -> cover 0 True "feature absent in era" True -- todo, is there a nice way to report these?
    Just xs' -> f xs'
