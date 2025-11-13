{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Constrained.Conway.MiniTrace (
  spec,
  ConstrainedGeneratorBundle (..),
  ConwayCertGenContext (..),
  constrainedCert,
  constrainedCerts,
  constrainedDeleg,
  constrainedEnact,
  constrainedEpoch,
  constrainedGov,
  constrainedGovCert,
  constrainedNewEpoch,
  constrainedPool,
  constrainedRatify,
  constrainedUtxo,
) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Network (..),
  ShelleyBase,
  addEpochInterval,
  natVersion,
 )
import Cardano.Ledger.Coin (Coin, CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  EnactState (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState,
  VotingProcedures,
 )
import Cardano.Ledger.Conway.PParams (ppCommitteeMaxTermLengthL)
import Cardano.Ledger.Conway.Rules (CertsEnv (..), EnactSignal)
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..), ConwayGovCert (..), ConwayTxCert (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State (
  CommitteeAuthorization (..),
  CommitteeState (..),
  DRep (..),
  EraCertState (..),
  IndividualPoolStake (..),
 )
import Constrained.API
import Control.State.Transition.Extended (STS (..))
import Data.Foldable (Foldable (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common hiding (forAll)
import Test.Cardano.Ledger.Constrained.Conway (
  EraSpecCert (..),
  UtxoExecContext,
  certEnvSpec,
  certsEnvSpec,
  coerce_,
  committeeMinSize_,
  conwayDelegCertSpec,
  conwayTxCertSpec,
  delegEnvSpec,
  epochSignalSpec,
  epochStateSpec,
  genUtxoExecContext,
  govCertSpec,
  govEnvSpec,
  govProceduresSpec,
  govProposalsSpec,
  newEpochStateSpec,
  pStateSpec,
  poolCertSpec,
  poolEnvSpec,
  protocolVersion_,
  txCertsSpec,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.GovCert (govCertEnvSpec)
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (
  conwayCertStateSpec,
  delegators,
  whoDelegatesSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse (WitUniv, genWitUniv)
import Test.Cardano.Ledger.Generic.Proof (goSTS)

data ConstrainedGeneratorBundle ctx rule era = ConstrainedGeneratorBundle
  { cgbContextGen :: Gen ctx
  , cgbEnvironmentSpec ::
      ctx ->
      Specification (Environment (EraRule rule era))
  , cgbStateSpec ::
      ctx ->
      Environment (EraRule rule era) ->
      Specification (State (EraRule rule era))
  , cgbSignalSpec ::
      ctx ->
      Environment (EraRule rule era) ->
      State (EraRule rule era) ->
      Specification (Signal (EraRule rule era))
  }

-- ====================================================================

-- | Generate either a list of signals, or a list of error messages
minitraceEither ::
  forall rule era ctx.
  ( HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  , HasSpec (Signal (EraRule rule era))
  , BaseM (EraRule rule era) ~ ShelleyBase
  , STS (EraRule rule era)
  , ToExpr (Signal (EraRule rule era))
  , ToExpr (State (EraRule rule era))
  ) =>
  Int ->
  ConstrainedGeneratorBundle ctx rule era ->
  Gen (Either [String] [Signal (EraRule rule era)])
minitraceEither n0 ConstrainedGeneratorBundle {..} = do
  !ctxt <- cgbContextGen
  !env <- genFromSpec $ cgbEnvironmentSpec ctxt
  !state <- genFromSpec $ cgbStateSpec ctxt env
  let go :: State (EraRule rule era) -> Int -> Gen (Either [String] [Signal (EraRule rule era)])
      go _ 0 = pure (Right [])
      go st n = do
        !signal <- genFromSpec $ cgbSignalSpec ctxt env st
        goSTS @rule @era @(Gen (Either [String] [Signal (EraRule rule era)]))
          env
          st
          signal
          ( \case
              Left ps ->
                pure
                  ( Left
                      ( [ "\nSIGNAL = " ++ show (toExpr signal)
                        , "\nSTATE = " ++ show (toExpr state)
                        , "\nPredicateFailures"
                        ]
                          ++ map show (NE.toList ps)
                      )
                  )
              Right !state2 -> do
                ans <- go state2 (n - 1)
                case ans of
                  Left xs -> pure (Left xs)
                  Right more -> pure (Right (signal : more))
          )
  go state n0

minitraceProp ::
  forall rule era ctx.
  ( HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  , HasSpec (Signal (EraRule rule era))
  , BaseM (EraRule rule era) ~ ShelleyBase
  , STS (EraRule rule era)
  , ToExpr (Signal (EraRule rule era))
  , ToExpr (State (EraRule rule era))
  ) =>
  Int ->
  ConstrainedGeneratorBundle ctx rule era ->
  (Signal (EraRule rule era) -> String) ->
  Gen Property
minitraceProp n0 cgb classifier = do
  ans <- minitraceEither @rule @era n0 cgb
  let
    classifyFirst _ [] p = p
    classifyFirst f (x : _) p = case f x of
      "" -> p
      cl -> classify True cl p
  case ans of
    Left zs -> pure $ counterexample (unlines zs) (property False)
    Right s -> pure . classifyFirst classifier s $ property True

runMinitrace ::
  forall (rule :: Symbol) era ctx.
  ( HasSpec (Environment (EraRule rule era))
  , HasSpec (State (EraRule rule era))
  , HasSpec (Signal (EraRule rule era))
  , BaseM (EraRule rule era) ~ ShelleyBase
  , KnownSymbol rule
  , STS (EraRule rule era)
  , ToExpr (Signal (EraRule rule era))
  , ToExpr (State (EraRule rule era))
  ) =>
  ConstrainedGeneratorBundle ctx rule era ->
  (Signal (EraRule rule era) -> String) ->
  Spec
runMinitrace cgb classifier =
  prop
    (symbolVal $ Proxy @rule)
    (withMaxSuccess 50 (minitraceProp @rule @era 50 cgb classifier))

genDelegCtx :: Gen (WitUniv ConwayEra, ConwayCertGenContext ConwayEra)
genDelegCtx = do
  witUniv <- genWitUniv 300
  ccec <- genFromSpec $ conwayCertExecContextSpec witUniv 4
  pure (witUniv, ccec)

certStateSpec_ ::
  EraSpecCert era =>
  (WitUniv era, ConwayCertGenContext era) -> Specification (CertState era)
certStateSpec_ (u, ConwayCertGenContext {..}) =
  certStateSpec u (Map.keysSet ccccDelegatees) ccccWithdrawals

ratifyEnvSpec ::
  [GovActionState era] ->
  Specification (RatifyEnv ConwayEra)
ratifyEnvSpec govActionMap =
  constrained' $ \_ poolDistr drepDistr drepState _ committeeState _ _ ->
    [ -- Bias the generator towards generating DReps that have stake and are registered
      exists
        ( \eval ->
            pure
              ( Set.intersection
                  (Map.keysSet (eval drepDistr))
                  (Set.map DRepCredential $ Map.keysSet (eval drepState))
              )
        )
        ( \common ->
            [ assert $ subset_ common (dom_ drepDistr)
            , reify drepState (Map.mapKeys DRepCredential) (assert . subset_ common . dom_)
            , drepDistr `dependsOn` common
            ]
        )
    , match poolDistr $ \poolStake _ ->
        exists
          ( \eval ->
              pure
                ( Set.intersection
                    (Map.keysSet $ eval poolStake)
                    spoVotes
                )
          )
          ( \common ->
              [ assert $ subset_ common (dom_ poolStake)
              , assert $ subset_ common (lit spoVotes)
              , poolStake `dependsOn` common
              ]
          )
    , match committeeState $ \ [var| cs |] ->
        exists
          ( \eval ->
              pure $
                Set.map CommitteeHotCredential ccVotes
                  `Set.intersection` foldr' Set.insert mempty (eval cs)
          )
          ( \ [var| common |] ->
              [ assert $ common `subset_` fromList_ (rng_ cs)
              , assert $ common `subset_` lit (Set.map CommitteeHotCredential ccVotes)
              , cs `dependsOn` common
              ]
          )
    , match poolDistr $ \ [var| individualStakesCompact |] [var| totalStakeCompact |] ->
        [ assert $
            reify
              individualStakesCompact
              (fmap (\IndividualPoolStake {individualTotalPoolStake = CompactCoin c} -> c) . Map.elems)
              ( \ [var| stakes |] ->
                  [ coerce_ totalStakeCompact ==. sum_ stakes
                  ]
              )
        , assert $ not_ (null_ individualStakesCompact)
        -- TODO make sure individual stakes add up to 1
        ]
    ]
  where
    spoVotes =
      foldr'
        ( \GovActionState {gasStakePoolVotes} s ->
            Map.keysSet gasStakePoolVotes <> s
        )
        mempty
        govActionMap
    ccVotes =
      foldr'
        ( \GovActionState {gasCommitteeVotes} s ->
            Map.keysSet gasCommitteeVotes <> s
        )
        mempty
        govActionMap

ratifyStateSpec ::
  RatifyEnv ConwayEra ->
  Specification (RatifyState ConwayEra)
ratifyStateSpec RatifyEnv {..} =
  constrained' $ \ens enacted expired _ ->
    mconcat
      [ assert $ enacted ==. lit mempty
      , assert $ expired ==. lit mempty
      , match ens $ \mbyCmt _ pp _ _ _ _ ->
          [ (caseOn mbyCmt)
              (branch $ \_ -> True)
              ( branch $ \cmt -> match cmt $ \cmtMap _ ->
                  exists
                    ( \eval ->
                        pure $
                          Set.intersection
                            ccColdKeys
                            (eval $ dom_ cmtMap)
                    )
                    ( \common ->
                        [ assert $ common `subset_` lit ccColdKeys
                        , assert $ common `subset_` dom_ cmtMap
                        , cmtMap `dependsOn` common
                        ]
                    )
              )
          , disableBootstrap pp
          , preferSmallerCCMinSizeValues pp
          ]
      ]
  where
    ccColdKeys =
      let CommitteeState m = reCommitteeState
       in Map.keysSet m
    -- Bootstrap is not in the spec
    disableBootstrap :: Term (PParams ConwayEra) -> Pred
    disableBootstrap pp = match pp $ \simplepp ->
      match (protocolVersion_ simplepp) $ \major _ ->
        assert $ not_ (major ==. lit (natVersion @9))

    preferSmallerCCMinSizeValues ::
      Term (PParams ConwayEra) ->
      Pred
    preferSmallerCCMinSizeValues pp = match pp $ \simplepp ->
      satisfies (committeeMinSize_ simplepp) $
        chooseSpec
          (1, trueSpec)
          (1, constrained (<=. committeeSize))
      where
        committeeSize = lit . fromIntegral . Set.size $ ccColdKeys

ratifySignalSpec ::
  [GovActionState ConwayEra] ->
  Specification (RatifySignal ConwayEra)
ratifySignalSpec govActions =
  constrained $ \sig ->
    match sig $ \gasS ->
      match gasS $ \gasL ->
        forAll gasL $ \gas ->
          gas `elem_` lit govActions

enactSignalSpec ::
  EpochNo ->
  EnactState ConwayEra ->
  Specification (EnactSignal ConwayEra)
enactSignalSpec epoch EnactState {..} =
  constrained' $ \_gid action ->
    [ -- TODO get rid of this by modifying the spec so that ENACT can't fail.
      -- Right now this constraint makes the generator avoid cases where
      -- the spec would fail, because such proposals would be handled in RATIFY
      -- and wouldn't make it to ENACT.
      (caseOn action)
        (branch $ \_ _ _ -> True)
        (branch $ \_ _ -> True)
        ( branch $ \newWdrls _ ->
            [ assert $ sum_ (rng_ newWdrls) + lit (sum ensWithdrawals) <=. lit ensTreasury
            , assert $ forAll' newWdrls $ \acct _ ->
                match acct $ \network _ ->
                  network ==. lit Testnet
            ]
        )
        (branch $ \_ -> True)
        ( branch $ \_ _ newMembers _ ->
            let expiry = addEpochInterval epoch (ensCurPParams ^. ppCommitteeMaxTermLengthL)
             in forAll (rng_ newMembers) (<=. lit expiry)
        )
        (branch $ \_ _ -> True)
        (branch $ \_ -> True)
    ]

enactStateSpec ::
  Specification (EnactState ConwayEra)
enactStateSpec =
  constrained' $ \_ _ _ _ treasury wdrls _ ->
    [ assert $ sum_ (rng_ wdrls) <=. treasury
    ]

namePoolCert :: PoolCert -> String
namePoolCert RegPool {} = "RegPool"
namePoolCert RetirePool {} = "RetirePool"

nameDelegCert :: ConwayDelegCert -> String
nameDelegCert ConwayRegCert {} = "RegKey"
nameDelegCert ConwayUnRegCert {} = "UnRegKey"
nameDelegCert ConwayDelegCert {} = "DelegateWithKey"
nameDelegCert ConwayRegDelegCert {} = "RegK&DelegateWithKey"

nameGovCert :: ConwayGovCert -> String
nameGovCert (ConwayRegDRep {}) = "ConwayRegDRep"
nameGovCert (ConwayUnRegDRep {}) = "ConwayUnRegDRep"
nameGovCert (ConwayUpdateDRep {}) = "ConwayUpdateDRep"
nameGovCert (ConwayAuthCommitteeHotKey {}) = "ConwayAuthCommitteeHotKey"
nameGovCert (ConwayResignCommitteeColdKey {}) = "ConwayResignCommitteeColdKey"

nameTxCert :: ConwayTxCert ConwayEra -> String
nameTxCert (ConwayTxCertDeleg x) = nameDelegCert x
nameTxCert (ConwayTxCertPool x) = namePoolCert x
nameTxCert (ConwayTxCertGov x) = nameGovCert x

nameCerts :: Seq (ConwayTxCert ConwayEra) -> String
nameCerts x = "Certs length " ++ show (length x)

-- | Run a minitrace for every instance of ExecRuleSpec
spec :: Spec
spec = do
  describe "50 MiniTrace tests with trace length of 50" $ do
    runMinitrace constrainedPool namePoolCert
    runMinitrace constrainedDeleg nameDelegCert
    runMinitrace constrainedGovCert nameGovCert
    runMinitrace constrainedCert nameTxCert
    runMinitrace constrainedCerts nameCerts
    runMinitrace constrainedRatify (const "")

    -- These properties do not have working 'signalSpec' Specifications yet.
    xdescribe "Pending tests" $ do
      runMinitrace constrainedGov (const "")
      runMinitrace constrainedUtxo (const "")
      runMinitrace constrainedEpoch (const "")
      runMinitrace constrainedNewEpoch (const "")
      runMinitrace @"ENACT" @ConwayEra
        constrainedEnact
        (const "")

data ConwayCertGenContext era = ConwayCertGenContext
  { ccccWithdrawals :: !(Map RewardAccount Coin)
  , ccccVotes :: !(VotingProcedures era)
  , ccccDelegatees :: !(Map (Credential DRepRole) (Set (Credential Staking)))
  }
  deriving (Generic, Eq, Show)

instance Era era => HasSimpleRep (ConwayCertGenContext era)

instance Era era => HasSpec (ConwayCertGenContext era)

-- | A Specification version of `Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs.genCertContext`
--   Makes sure all the subtle invariants of `type WhoDelegates` are met.
conwayCertExecContextSpec ::
  forall era. Era era => WitUniv era -> Integer -> Specification (ConwayCertGenContext era)
conwayCertExecContextSpec univ wdrlsize = constrained' $
  \ [var|withdrawals|] _voteProcs [var|delegatees|] ->
    [ withdrawals `dependsOn` delegatees
    , satisfies delegatees (whoDelegatesSpec univ)
    , assert
        [ assert $ sizeOf_ withdrawals <=. lit wdrlsize
        , reify delegatees delegators $ \ [var|credStakeSet|] ->
            [ forAll' withdrawals $ \ [var|rewAccount|] [var|_wCoin|] ->
                match rewAccount $ \ [var|network|] [var|rewcred|] ->
                  [network ==. lit Testnet, member_ rewcred credStakeSet]
            ]
        ]
    ]

-- Constrained generator bundles for each rule

constrainedPool :: ConstrainedGeneratorBundle (WitUniv ConwayEra) "POOL" ConwayEra
constrainedPool =
  ConstrainedGeneratorBundle
    (genWitUniv 50)
    poolEnvSpec
    (\ctx _ -> pStateSpec ctx)
    poolCertSpec

constrainedDeleg ::
  ConstrainedGeneratorBundle
    (WitUniv ConwayEra, ConwayCertGenContext ConwayEra)
    "DELEG"
    ConwayEra
constrainedDeleg =
  ConstrainedGeneratorBundle
    genDelegCtx
    (const delegEnvSpec)
    (\ctx _ -> certStateSpec_ ctx)
    (const conwayDelegCertSpec)

constrainedGovCert ::
  ConstrainedGeneratorBundle
    (WitUniv ConwayEra, ConwayCertGenContext ConwayEra)
    "GOVCERT"
    ConwayEra
constrainedGovCert =
  ConstrainedGeneratorBundle
    genDelegCtx
    (\(u, _) -> govCertEnvSpec u)
    (\ctx _ -> certStateSpec_ ctx)
    (\(u, _) -> govCertSpec u)

constrainedCert ::
  ConstrainedGeneratorBundle
    (WitUniv ConwayEra, ConwayCertGenContext ConwayEra)
    "CERT"
    ConwayEra
constrainedCert =
  ConstrainedGeneratorBundle
    genDelegCtx
    (\(u, _) -> certEnvSpec u)
    (\ctx _ -> certStateSpec_ ctx)
    (\(u, _) -> conwayTxCertSpec u)

constrainedCerts ::
  ConstrainedGeneratorBundle
    (WitUniv ConwayEra, ConwayCertGenContext ConwayEra)
    "CERTS"
    ConwayEra
constrainedCerts =
  ConstrainedGeneratorBundle
    genDelegCtx
    (const certsEnvSpec)
    ( \(u, ConwayCertGenContext {..}) CertsEnv {certsCurrentEpoch} ->
        conwayCertStateSpec u (ccccDelegatees, ccccWithdrawals) (lit certsCurrentEpoch)
    )
    (\(u, _) -> txCertsSpec u)

constrainedRatify :: ConstrainedGeneratorBundle [GovActionState ConwayEra] "RATIFY" ConwayEra
constrainedRatify =
  ConstrainedGeneratorBundle
    arbitrary
    ratifyEnvSpec
    (\_ env -> ratifyStateSpec env)
    (\ctx _ _ -> ratifySignalSpec ctx)

constrainedGov :: ConstrainedGeneratorBundle () "GOV" ConwayEra
constrainedGov =
  ConstrainedGeneratorBundle
    (pure ())
    (const govEnvSpec)
    (const govProposalsSpec)
    (const govProceduresSpec)

constrainedUtxo ::
  ConstrainedGeneratorBundle (UtxoExecContext ConwayEra) "UTXO" ConwayEra
constrainedUtxo =
  ConstrainedGeneratorBundle
    genUtxoExecContext
    utxoEnvSpec
    utxoStateSpec
    (\ctx _ _ -> utxoTxSpec ctx)

constrainedEpoch :: ConstrainedGeneratorBundle EpochNo "EPOCH" ConwayEra
constrainedEpoch =
  ConstrainedGeneratorBundle
    arbitrary
    (const trueSpec)
    (\ctx _ -> epochStateSpec (lit ctx))
    (\ctx _ _ -> epochSignalSpec ctx)

constrainedNewEpoch :: ConstrainedGeneratorBundle EpochNo "NEWEPOCH" ConwayEra
constrainedNewEpoch =
  ConstrainedGeneratorBundle
    arbitrary
    (const trueSpec)
    (\_ _ -> newEpochStateSpec)
    (\ctx _ _ -> epochSignalSpec ctx)

constrainedEnact :: ConstrainedGeneratorBundle EpochNo "ENACT" ConwayEra
constrainedEnact =
  ConstrainedGeneratorBundle
    arbitrary
    (const trueSpec)
    (\_ _ -> enactStateSpec)
    (\epoch _ st -> enactSignalSpec epoch st)
