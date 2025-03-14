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
import Constrained.TheKnot (shrinkWithSpec)
import Control.Monad.Reader
import Control.State.Transition.Extended
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import Test.Cardano.Ledger.Constrained.Conway.Cert
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.Epoch
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Pool
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse (WitUniv, genWitUniv, witness)
import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Shelley.Utils
import Test.QuickCheck hiding (witness)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (witness)

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
  wdrls <- genFromSpec (constrained $ \x -> (witness univ x))
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
  , PrettyA st
  , PrettyA sig
  , PrettyA env
  , PrettyA fail
  , Testable p
  , HasSpec env
  , HasSpec st
  , HasSpec sig
  ) =>
  Specification env ->
  (env -> Specification st) ->
  (env -> st -> Specification sig) ->
  (env -> st -> sig -> st -> p) ->
  Property
stsPropertyV2 specEnv specState specSig prop =
  stsPropertyV2' @r specEnv specState specSig (\env _ _ -> specState env) prop

stsPropertyV2' ::
  forall r env st sig fail p.
  ( Environment (EraRule r ConwayEra) ~ env
  , State (EraRule r ConwayEra) ~ st
  , Signal (EraRule r ConwayEra) ~ sig
  , PredicateFailure (EraRule r ConwayEra) ~ fail
  , STS (EraRule r ConwayEra)
  , BaseM (EraRule r ConwayEra) ~ ReaderT Globals Identity
  , PrettyA st
  , PrettyA sig
  , PrettyA env
  , PrettyA fail
  , Testable p
  , HasSpec env
  , HasSpec st
  , HasSpec sig
  ) =>
  Specification env ->
  (env -> Specification st) ->
  (env -> st -> Specification sig) ->
  -- This allows you to write a separate spec for the state after the transition
  -- and thus e.g. loosening requirements set only for the sake of generation
  (env -> st -> sig -> Specification st) ->
  (env -> st -> sig -> st -> p) ->
  Property
stsPropertyV2' specEnv specState specSig specPostState prop =
  uncurry forAllShrinkBlind (genShrinkFromSpec specEnv) $ \env ->
    counterexample (show $ ppString "env = " <> prettyA env) $
      uncurry forAllShrinkBlind (genShrinkFromSpec $ specState env) $ \st ->
        counterexample (show $ ppString "st = " <> prettyA st) $
          uncurry forAllShrinkBlind (genShrinkFromSpec $ specSig env st) $ \sig ->
            counterexample (show $ ppString "sig = " <> prettyA sig) $
              runShelleyBase $ do
                res <- applySTS @(EraRule r ConwayEra) $ TRC (env, st, sig)
                pure $ case res of
                  Left pfailures -> counterexample (show $ prettyA pfailures) $ property False
                  Right st' ->
                    case conformsToSpecE
                      st
                      (specPostState env st sig)
                      (pure "conformsToSpecE fails in STS tests") of
                      Just es -> counterexample (unlines (NE.toList es)) False
                      Nothing ->
                        counterexample
                          ( show $
                              ppString "st' = "
                                <> prettyA st'
                                <> ppString ("\nspec = \n" ++ show (specState env))
                          )
                          $ prop env st sig st'

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
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

prop_EPOCH :: EpochNo -> Property
prop_EPOCH epochNo =
  stsPropertyV2' @"EPOCH"
    TrueSpec
    (\_env -> epochStateSpec (lit epochNo))
    (\_env _st -> epochSignalSpec epochNo)
    (\_env _st _newEpoch -> TrueSpec)
    -- (\_env _st newEpoch -> epochStateSpec (lit newEpoch))
    $ \_env _st _sig _st' -> True

prop_ENACT :: Property
prop_ENACT =
  stsPropertyV2 @"ENACT"
    TrueSpec
    (\_env -> TrueSpec)
    -- TODO: this is a bit suspect, there are preconditions on these signals in the spec so we
    -- shouldn't expect this to go through so easily.
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

prop_UTXOS :: Property
prop_UTXOS =
  stsPropertyV2 @"UTXOS"
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

-- prop_LEDGER :: Property
-- prop_LEDGER = property $ do
--  pure $ stsPropertyV2 @"LEDGER"
--    TrueSpec
--    (\_env -> TrueSpec)
--    -- TODO: the `GenDelegs` don't appear to be used (?!) so we just give an
--    -- empty map here. One could consider generating them instead
--    ledgerTxSpec
--    $ \_env _st _sig _st' -> True

-- prop_TICKF :: Property
-- prop_TICKF =
--   stsPropertyV2 @"TICKF"
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

prop_RATIFY :: Property
prop_RATIFY =
  stsPropertyV2 @"RATIFY"
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
    -- TODO: we should probably check more things here
    $ \_env _st _sig _st' -> True

-- prop_CERTS :: Property
-- prop_CERTS =
--   stsPropertyV2 @"CERTS"
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
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
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
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
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_LEDGERS :: Property
-- prop_LEDGERS =
--   stsPropertyV2 @"LEDGERS"
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_RUPD :: Property
-- prop_RUPD =
--   stsPropertyV2 @"RUPD"
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_SNAP :: Property
-- prop_SNAP =
--   stsPropertyV2 @"SNAP"
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_TICK :: Property
-- prop_TICK =
--   stsPropertyV2 @"TICK"
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

------------------------------------------------------------------------
-- Test Tree
------------------------------------------------------------------------

tests_STS :: TestTree
tests_STS =
  testGroup
    "STS property tests"
    [ govTests
    , -- , utxoTests
      -- TODO: this is probably one of the last things we want to
      -- get passing as it depends on being able to generate a complete
      -- `EpochState era`
      testProperty "prop_EPOCH" prop_EPOCH
      -- , testProperty "prop_LEDGER" prop_LEDGER
    ]

govTests :: TestTree
govTests =
  testGroup
    "GOV tests"
    [ testProperty "prop_GOVCERT" prop_GOVCERT
    , testProperty "prop_POOL" prop_POOL
    , testProperty "prop_DELEG" prop_DELEG
    , testProperty "prop_ENACT" prop_ENACT
    , testProperty "prop_RATIFY" prop_RATIFY
    , testProperty "prop_CERT" prop_CERT
    , testProperty "prop_GOV" prop_GOV
    ]

utxoTests :: TestTree
utxoTests =
  testGroup
    "UTXO* rules"
    [ {-testProperty "prop_UTXO" prop_UTXO
      ,-} testProperty "prop_UTXOW" prop_UTXOW
    , testProperty "prop_UTXOS" prop_UTXOS
    ]

epoch :: TestTree
epoch =
  testGroup
    "STS property tests"
    [ govTests
    , testProperty "prop_EPOCH" prop_EPOCH
    ]
