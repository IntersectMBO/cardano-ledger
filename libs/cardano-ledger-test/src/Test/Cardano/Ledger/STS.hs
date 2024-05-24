{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.STS where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Shelley.Rules hiding (epochNo, slotNo)
import Control.Monad.Reader
import Control.State.Transition.Extended

import Constrained

import Test.Cardano.Ledger.Constrained.Conway.NES
import Test.Cardano.Ledger.Constrained.Conway.Cert
import Test.Cardano.Ledger.Constrained.Conway.Deleg
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Test.Cardano.Ledger.Constrained.Conway.GovCert
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Ledger
import Test.Cardano.Ledger.Constrained.Conway.Pool
import Test.Cardano.Ledger.Constrained.Conway.Utxo

import Test.Cardano.Ledger.Generic.PrettyCore
import Test.Cardano.Ledger.Shelley.Utils

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------

type GenShrink a = (Gen a, a -> [a])

genShrinkFromSpec :: forall fn a. HasSpec fn a => Specification fn a -> GenShrink a
genShrinkFromSpec spec = (genFromSpec @fn spec, shrinkWithSpec @fn spec)

stsPropertyV2 ::
  forall r fn era env st sig fail p.
  ( era ~ ConwayEra StandardCrypto
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
  , HasSpec fn env
  , HasSpec fn st
  , HasSpec fn sig
  ) =>
  Specification fn env ->
  (env -> Specification fn st) ->
  (env -> st -> Specification fn sig) ->
  (env -> st -> sig -> st -> p) ->
  Property
stsPropertyV2 specEnv specState specSig prop =
  uncurry forAllShrinkBlind (genShrinkFromSpec specEnv) $ \env ->
    counterexample (show $ ppString "env = " <> prettyA env) $
      uncurry forAllShrinkBlind (genShrinkFromSpec $ specState env) $ \st ->
        counterexample (show $ ppString "st = " <> prettyA st) $
          uncurry forAllShrinkBlind (genShrinkFromSpec $ specSig env st) $ \sig ->
            counterexample (show $ ppString "sig = " <> prettyA sig) $
              runShelleyBase $ do
                res <- applySTS @(EraRule r era) $ TRC (env, st, sig)
                pure $ case res of
                  Left pfailures -> counterexample (show $ prettyA pfailures) $ property False
                  Right st' ->
                    counterexample (show $ ppString "st' = " <> prettyA st') $
                      conformsToSpec @fn st' (specState env)
                        .&&. prop env st sig st'

-- STS properties ---------------------------------------------------------

prop_GOV :: Property
prop_GOV =
  stsPropertyV2 @"GOV" @ConwayFn
    govEnvSpec
    govProposalsSpecSTS
    govProceduresSpec
    -- TODO: we should probably check more things here
    $ \_env _st _sig _st' -> True

-- prop_NEWEPOCH :: Property
-- prop_NEWEPOCH =
--   stsPropertyV2 @"NEWEPOCH" @ConwayFn
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

prop_EPOCH :: Property
prop_EPOCH = stsPropertyV2 @"EPOCH" @ConwayFn
    TrueSpec
    (\ () -> esSpecSTS)
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

prop_ENACT :: Property
prop_ENACT =
  stsPropertyV2 @"ENACT" @ConwayFn
    TrueSpec
    (\_env -> TrueSpec)
    -- TODO: this is a bit suspect, there are preconditions on these signals in the spec so we
    -- shouldn't expect this to go through so easily.
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

prop_UTXOS :: Property
prop_UTXOS =
  stsPropertyV2 @"UTXOS" @ConwayFn
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

prop_LEDGER :: Property
prop_LEDGER =
  stsPropertyV2 @"LEDGER" @ConwayFn
    TrueSpec
    (\_env -> TrueSpec)
    -- TODO: the `GenDelegs` don't appear to be used (?!) so we just give an
    -- empty map here. One could consider generating them instead
    ledgerTxSpec
    $ \_env _st _sig _st' -> True

-- prop_TICKF :: Property
-- prop_TICKF =
--   stsPropertyV2 @"TICKF" @ConwayFn
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

prop_RATIFY :: Property
prop_RATIFY =
  stsPropertyV2 @"RATIFY" @ConwayFn
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
    -- TODO: we should probably check more things here
    $ \_env _st _sig _st' -> True

-- prop_CERTS :: Property
-- prop_CERTS =
--   stsPropertyV2 @"CERTS" @ConwayFn
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     -- TODO: we should probably check more things here
--     $ \_env _st _sig _st' -> True

prop_CERT :: Property
prop_CERT =
  stsPropertyV2 @"CERT" @ConwayFn
    certEnvSpec
    (\_env -> certStateSpec)
    (\env st -> txCertSpec env st)
    -- TODO: we should probably check more things here
    $ \_env _st _sig _st' -> True

prop_DELEG :: Property
prop_DELEG =
  stsPropertyV2 @"DELEG" @ConwayFn
    delegEnvSpec
    (\_env -> dStateSpec)
    delegCertSpec
    $ \_env _st _sig _st' -> True

prop_POOL :: Property
prop_POOL =
  stsPropertyV2 @"POOL" @ConwayFn
    poolEnvSpec
    (\_env -> pStateSpec)
    (\env st -> poolCertSpec env st)
    $ \_env _st _sig _st' -> True

prop_GOVCERT :: Property
prop_GOVCERT =
  stsPropertyV2 @"GOVCERT" @ConwayFn
    govCertEnvSpec
    (\_env -> vStateSpec)
    (\env st -> govCertSpec env st)
    $ \_env _st _sig _st' -> True

prop_UTXOW :: Property
prop_UTXOW =
  stsPropertyV2 @"UTXOW" @ConwayFn
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

prop_UTXO :: Property
prop_UTXO =
  stsPropertyV2 @"UTXO" @ConwayFn
    utxoEnvSpec
    utxoStateSpec
    utxoTxSpec
    $ \_env _st _sig _st' -> True

-- prop_BBODY :: Property
-- prop_BBODY =
--   stsPropertyV2 @"BBODY" @ConwayFn
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_LEDGERS :: Property
-- prop_LEDGERS =
--   stsPropertyV2 @"LEDGERS" @ConwayFn
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

-- prop_RUPD :: Property
-- prop_RUPD =
--   stsPropertyV2 @"RUPD" @ConwayFn
--     TrueSpec
--     (\_env -> TrueSpec)
--     (\_env _st -> TrueSpec)
--     $ \_env _st _sig _st' -> True

prop_SNAP :: Property
prop_SNAP =
  stsPropertyV2 @"SNAP" @ConwayFn
    TrueSpec
    (\_env -> TrueSpec)
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

prop_TICK :: Property
prop_TICK =
  stsPropertyV2 @"TICK" @ConwayFn
    TrueSpec
    (\_env -> nesSpec)
    (\_env _st -> TrueSpec)
    $ \_env _st _sig _st' -> True

------------------------------------------------------------------------
-- Test Tree
------------------------------------------------------------------------

tests_STS :: TestTree
tests_STS =
  testGroup
    "STS property tests"
    [ govTests
    , testProperty "prop_SNAP" prop_SNAP
    , testProperty "prop_TICK" prop_SNAP
    -- , utxoTests
    -- TODO: this is probably one of the last things we want to
    -- get passing as it depends on being able to generate a complete
    -- `EpochState era`
    -- , testProperty "prop_EPOCH" prop_EPOCH
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
    [ testProperty "prop_UTXO" prop_UTXO
    , testProperty "prop_UTXOW" prop_UTXOW
    , testProperty "prop_UTXOS" prop_UTXOS
    ]

epoch :: TestTree
epoch =
  testGroup
    "STS property tests"
    [ govTests
    , testProperty "prop_EPOCH" prop_EPOCH
    ]
