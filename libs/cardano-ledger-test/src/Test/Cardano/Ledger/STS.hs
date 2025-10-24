{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.STS where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Shelley.Rules hiding (epochNo, slotNo)
import Constrained.API hiding (forAll)
import Control.Monad.Reader
import Control.State.Transition.Extended
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import Test.Cardano.Ledger.Common hiding (witness)
import Test.Cardano.Ledger.Constrained.Conway.Cert
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.Epoch
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Pool
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse (WitUniv, genWitUniv, witness)
import Test.Cardano.Ledger.Shelley.Utils

-- ==================================================

-- | Several tests need some semi-randomcontext, this supplies that context
genContext ::
  Gen
    ( WitUniv ConwayEra
    , Set (Credential 'DRepRole)
    , Map RewardAccount Coin
    )
genContext = do
  univ <- genWitUniv @ConwayEra 200
  delegatees <- genFromSpec (delegateeSpec univ)
  wdrls <- genFromSpec (constrained $ \x -> witness univ x)
  pure (univ, delegatees, wdrls)

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

type GenShrink a = (Gen a, a -> [a])

genShrinkFromSpec :: forall a. HasSpec a => Specification a -> GenShrink a
genShrinkFromSpec spec = (genFromSpec spec, shrinkWithSpec spec)

stsPropertyV2 ::
  forall r era env st sig fail p.
  ( era ~ ConwayEra
  , Environment (EraRule r era) ~ env
  , State (EraRule r era) ~ st
  , Signal (EraRule r era) ~ sig
  , PredicateFailure (EraRule r era) ~ fail
  , STS (EraRule r era)
  , BaseM (EraRule r era) ~ ReaderT Globals Identity
  , Testable p
  , HasSpec env
  , HasSpec st
  , HasSpec sig
  , ToExpr env
  , ToExpr st
  , ToExpr sig
  , ToExpr fail
  ) =>
  Specification env ->
  (env -> Specification st) ->
  (env -> st -> Specification sig) ->
  (env -> st -> sig -> st -> p) ->
  Property
stsPropertyV2 specEnv specState specSig =
  stsPropertyV2' @r specEnv specState specSig (\env _ _ -> specState env)

stsPropertyV2' ::
  forall r env st sig fail p.
  ( Environment (EraRule r ConwayEra) ~ env
  , State (EraRule r ConwayEra) ~ st
  , Signal (EraRule r ConwayEra) ~ sig
  , PredicateFailure (EraRule r ConwayEra) ~ fail
  , STS (EraRule r ConwayEra)
  , BaseM (EraRule r ConwayEra) ~ ReaderT Globals Identity
  , Testable p
  , HasSpec env
  , HasSpec st
  , HasSpec sig
  , ToExpr env
  , ToExpr st
  , ToExpr sig
  , ToExpr fail
  ) =>
  Specification env ->
  (env -> Specification st) ->
  (env -> st -> Specification sig) ->
  -- This allows you to write a separate spec for the state after the transition
  -- and thus e.g. loosening requirements set only for the sake of generation
  (env -> st -> sig -> Specification st) ->
  (env -> st -> sig -> st -> p) ->
  Property
stsPropertyV2' specEnv specState specSig specPostState theProp =
  uncurry forAllShrinkBlind (genShrinkFromSpec specEnv) $ \env ->
    counterexample (show $ toExpr env) $
      uncurry forAllShrinkBlind (genShrinkFromSpec $ specState env) $ \st ->
        counterexample (show $ toExpr st) $
          uncurry forAllShrinkBlind (genShrinkFromSpec $ specSig env st) $ \sig ->
            counterexample (show $ toExpr sig) $
              runShelleyBase $ do
                res <- applySTS @(EraRule r ConwayEra) $ TRC (env, st, sig)
                pure $ case res of
                  Left pfailures -> counterexample (show $ toExpr pfailures) $ property False
                  Right st' ->
                    case conformsToSpecE
                      st
                      (specPostState env st sig)
                      (pure "conformsToSpecE fails in STS tests") of
                      Just es -> counterexample (unlines (NE.toList es)) False
                      Nothing ->
                        counterexample
                          ( show
                              (toExpr st', show (specState env))
                          )
                          $ theProp env st sig st'

-- STS properties ---------------------------------------------------------

prop_GOV :: Property
prop_GOV =
  stsPropertyV2 @"GOV"
    govEnvSpec
    govProposalsSpec
    govProceduresSpec
    -- TODO: we should probably check more things here
    $ \_env _st _sig _st' -> True

-- prop_NEWEPOCH :: Property
-- prop_NEWEPOCH =
--   stsPropertyV2 @"NEWEPOCH"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

prop_EPOCH :: EpochNo -> Property
prop_EPOCH epochNo =
  stsPropertyV2' @"EPOCH"
    trueSpec
    (\_env -> epochStateSpec (lit epochNo))
    (\_env _st -> epochSignalSpec epochNo)
    (\_env _st _newEpoch -> trueSpec)
    -- (\_env _st newEpoch -> epochStateSpec (lit newEpoch))
    $ \_env _st _sig _st' -> True

prop_ENACT :: Property
prop_ENACT =
  stsPropertyV2 @"ENACT"
    trueSpec
    (\_env -> trueSpec)
    -- TODO: this is a bit suspect, there are preconditions on these signals in the spec so we
    -- shouldn't expect this to go through so easily.
    (\_env _st -> trueSpec)
    $ \_env _st _sig _st' -> True

prop_UTXOS :: Property
prop_UTXOS =
  stsPropertyV2 @"UTXOS"
    trueSpec
    (\_env -> trueSpec)
    (\_env _st -> trueSpec)
    $ \_env _st _sig _st' -> True

-- prop_LEDGER :: Property
-- prop_LEDGER = property $ do
--  pure $ stsPropertyV2 @"LEDGER"
--    trueSpec
--    (\_env -> trueSpec)
--    -- TODO: the `GenDelegs` don't appear to be used (?!) so we just give an
--    -- empty map here. One could consider generating them instead
--    ledgerTxSpec
--    $ \_env _st _sig _st' -> True

-- prop_TICKF :: Property
-- prop_TICKF =
--   stsPropertyV2 @"TICKF"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

prop_RATIFY :: Property
prop_RATIFY =
  stsPropertyV2 @"RATIFY"
    trueSpec
    (\_env -> trueSpec)
    (\_env _st -> trueSpec)
    -- TODO: we should probably check more things here
    $ \_env _st _sig _st' -> True

-- prop_CERTS :: Property
-- prop_CERTS =
--   stsPropertyV2 @"CERTS"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     -- TODO: we should probably check more things here
--     $ \_env _st _sig _st' -> True

prop_CERT :: Property
prop_CERT =
  forAll
    genContext
    ( \(conwayWitUniv, conwayDelegatees, conwayWdrls) ->
        stsPropertyV2 @"CERT"
          (certEnvSpec conwayWitUniv)
          (\_env -> certStateSpec conwayWitUniv conwayDelegatees conwayWdrls)
          (\env st -> conwayTxCertSpec conwayWitUniv env st)
          -- TODO: we should probably check more things here
          $ \_env _st _sig _st' -> True
    )

prop_DELEG :: Property
prop_DELEG =
  forAll
    genContext
    ( \(conwayWitUniv, conwayDelegatees, conwayWdrls) ->
        stsPropertyV2 @"DELEG"
          delegEnvSpec
          (\_env -> certStateSpec conwayWitUniv conwayDelegatees conwayWdrls)
          conwayDelegCertSpec
          $ \_env _st _sig _st' -> True
    )

prop_POOL :: Property
prop_POOL =
  forAll
    genContext
    ( \(conwayWitUniv, _, _) ->
        stsPropertyV2 @"POOL"
          (poolEnvSpec conwayWitUniv)
          (\_env -> pStateSpec conwayWitUniv)
          (\env st -> poolCertSpec @ConwayEra conwayWitUniv env st)
          $ \_env _st _sig _st' -> True
    )

prop_GOVCERT :: Property
prop_GOVCERT =
  forAll
    genContext
    ( \(conwayWitUniv, conwayDelegatees, conwayWdrls) ->
        stsPropertyV2 @"GOVCERT"
          (govCertEnvSpec conwayWitUniv)
          (\_env -> certStateSpec conwayWitUniv conwayDelegatees conwayWdrls)
          (\env st -> govCertSpec conwayWitUniv env st)
          $ \_env _st _sig _st' -> True
    )

prop_UTXOW :: Property
prop_UTXOW =
  stsPropertyV2 @"UTXOW"
    trueSpec
    (\_env -> trueSpec)
    (\_env _st -> trueSpec)
    $ \_env _st _sig _st' -> True

-- prop_UTXO :: Property
-- prop_UTXO = property $ do
--  ctx <- arbitrary
--  pure $ stsPropertyV2 @"UTXO"
--    utxoEnvSpec
--    utxoStateSpec
--    (utxoTxSpec ctx)
--    $ \_env _st _sig _st' -> True

-- prop_BBODY :: Property
-- prop_BBODY =
--   stsPropertyV2 @"BBODY"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_LEDGERS :: Property
-- prop_LEDGERS =
--   stsPropertyV2 @"LEDGERS"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_RUPD :: Property
-- prop_RUPD =
--   stsPropertyV2 @"RUPD"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_SNAP :: Property
-- prop_SNAP =
--   stsPropertyV2 @"SNAP"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_TICK :: Property
-- prop_TICK =
--   stsPropertyV2 @"TICK"
--     trueSpec
--     (\_env -> trueSpec)
--     (\_env _st -> trueSpec)
--     $ \_env _st _sig _st' -> True

------------------------------------------------------------------------
-- Test Tree
------------------------------------------------------------------------

tests_STS :: Spec
tests_STS =
  describe "STS property tests" $ do
    govTests
    -- utxoTests
    -- prop "prop_LEDGER" prop_LEDGER
    -- TODO: this is probably one of the last things we want to
    -- get passing as it depends on being able to generate a complete
    -- `EpochState era`
    prop "prop_EPOCH" prop_EPOCH

govTests :: Spec
govTests =
  describe "GOV tests" $ do
    prop "prop_GOVCERT" prop_GOVCERT
    prop "prop_POOL" prop_POOL
    prop "prop_DELEG" prop_DELEG
    prop "prop_ENACT" prop_ENACT
    prop "prop_RATIFY" prop_RATIFY
    prop "prop_CERT" prop_CERT
    prop "prop_GOV" prop_GOV

utxoTests :: Spec
utxoTests =
  describe "UTXO* rules" $ do
    -- prop "prop_UTXO" prop_UTXO
    prop "prop_UTXOW" prop_UTXOW
    prop "prop_UTXOS" prop_UTXOS

epoch :: Spec
epoch =
  describe "STS property tests" $ do
    govTests
    prop "prop_EPOCH" prop_EPOCH
