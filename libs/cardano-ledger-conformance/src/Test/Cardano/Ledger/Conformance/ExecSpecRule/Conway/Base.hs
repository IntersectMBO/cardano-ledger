{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  nameEpoch,
  nameEnact,
  nameGovAction,
  ConwayRatifyExecContext (..),
) where

import Cardano.Ledger.BaseTypes (
  EpochNo (..),
  Inject (..),
  StrictMaybe (..),
  natVersion,
 )
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  CommitteeState (..),
  authorizedHotCommitteeCredentials,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (Era (..), EraPParams (..), PParams)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  gasAction,
 )
import Cardano.Ledger.Conway.Rules (
  EnactSignal (..),
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  spoAccepted,
  spoAcceptedRatio,
 )
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Constrained
import Constrained.Base (fromList_)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Lib as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..))
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  OpaqueErrorString (..),
  SpecTranslate (..),
  computationResultToEither,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (defaultTestConformance)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (ConwayExecEnactEnv)
import Test.Cardano.Ledger.Constrained.Conway (
  EpochExecEnv,
  IsConwayUniv,
  coerce_,
  epochEnvSpec,
  epochSignalSpec,
  epochStateSpec,
  newEpochStateSpec,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common hiding (arbitrary, forAll, prop, var)

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "UTXO" Conway
  where
  environmentSpec _ = utxoEnvSpec

  stateSpec _ = utxoStateSpec

  signalSpec _ env st =
    utxoTxSpec env st
      <> constrained disableInlineDatums
    where
      disableInlineDatums :: Term fn (AlonzoTx Conway) -> Pred fn
      disableInlineDatums tx = match @fn tx $ \txBody _ _ _ ->
        match txBody $
          \_ctbSpendInputs
           _ctbCollateralInputs
           _ctbReferenceInputs
           ctbOutputs
           _ctbCollateralReturn
           _ctbTotalCollateral
           _ctbCerts
           _ctbWithdrawals
           _ctbTxfee
           _ctbVldt
           _ctbReqSignerHashes
           _ctbMint
           _ctbScriptIntegrityHash
           _ctbAdHash
           _ctbTxNetworkId
           _ctbVotingProcedures
           _ctbProposalProcedures
           _ctbCurrentTreasuryValue
           _ctbTreasuryDonation ->
              match ctbOutputs $
                \outs -> forAll' outs $
                  \txOut _ -> match txOut $
                    \_ _ dat _ ->
                      (caseOn dat)
                        (branch $ \_ -> True)
                        (branch $ \_ -> True)
                        (branch $ \_ -> False)

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.utxoStep env st sig

data ConwayRatifyExecContext era = ConwayRatifyExecContext
  { crecTreasury :: Coin
  , crecGovActionMap :: [GovActionState era]
  }
  deriving (Generic, Eq, Show)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (ConwayRatifyExecContext era)

instance
  ( Era era
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (ConwayRatifyExecContext era)
  where
  arbitrary =
    ConwayRatifyExecContext
      <$> arbitrary
      <*> arbitrary

  shrink = genericShrink

instance Inject (ConwayRatifyExecContext era) Coin where
  inject = crecTreasury

instance
  Inject
    (ConwayRatifyExecContext era)
    [GovActionState era]
  where
  inject = crecGovActionMap

instance HasSimpleRep (ConwayRatifyExecContext era)
instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSpec fn (GovActionState era)
  ) =>
  HasSpec fn (ConwayRatifyExecContext era)

instance EraPParams era => NFData (ConwayRatifyExecContext era)

ratifyEnvSpec ::
  ( IsConwayUniv fn
  , HasSpec fn (RatifyEnv Conway)
  , HasSpec fn (SimpleRep (RatifyEnv Conway))
  ) =>
  ConwayRatifyExecContext Conway ->
  Specification fn (RatifyEnv Conway)
ratifyEnvSpec ConwayRatifyExecContext {crecGovActionMap} =
  constrained' $ \_ poolDistr drepDistr drepState _ committeeState ->
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
                Set.map
                  CommitteeHotCredential
                  ( Set.intersection
                      (authorizedHotCommitteeCredentials (eval committeeState))
                      ccVotes
                  )
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
        crecGovActionMap
    ccVotes =
      foldr'
        ( \GovActionState {gasCommitteeVotes} s ->
            Map.keysSet gasCommitteeVotes <> s
        )
        mempty
        crecGovActionMap

ratifyStateSpec ::
  IsConwayUniv fn =>
  ConwayRatifyExecContext Conway ->
  RatifyEnv Conway ->
  Specification fn (RatifyState Conway)
ratifyStateSpec _ RatifyEnv {..} =
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
          ]
      ]
  where
    ccColdKeys =
      let CommitteeState m = reCommitteeState
       in Map.keysSet m
    -- Bootstrap is not in the spec
    disableBootstrap :: IsConwayUniv fn => Term fn (PParams Conway) -> Pred fn
    disableBootstrap pp = match pp $ \pp' ->
      match (sel @12 pp') $ \major _ ->
        assert $ not_ (major ==. lit (natVersion @9))

ratifySignalSpec ::
  IsConwayUniv fn =>
  ConwayRatifyExecContext Conway ->
  Specification fn (RatifySignal Conway)
ratifySignalSpec ConwayRatifyExecContext {crecGovActionMap} =
  constrained $ \sig ->
    match sig $ \gasS ->
      match gasS $ \gasL ->
        forAll gasL $ \gas ->
          gas `elem_` lit crecGovActionMap

instance IsConwayUniv fn => ExecSpecRule fn "RATIFY" Conway where
  type ExecContext fn "RATIFY" Conway = ConwayRatifyExecContext Conway

  genExecContext = arbitrary

  environmentSpec = ratifyEnvSpec

  stateSpec = ratifyStateSpec

  signalSpec ctx _env _st = ratifySignalSpec ctx

  runAgdaRule env st sig =
    first (\case {})
      . computationResultToEither
      $ Agda.ratifyStep env st sig

  extraInfo ctx env@RatifyEnv {..} st sig@(RatifySignal actions) =
    unlines . toList $ actionAcceptedRatio <$> actions
    where
      members = foldMap' (committeeMembers @Conway) $ ensCommittee (rsEnactState st)
      showAccepted True = "✓"
      showAccepted False = "×"
      actionAcceptedRatio gas@GovActionState {..} =
        unlines
          [ "Acceptance ratios (impl):"
          , "GovActionId: \t" <> showExpr gasId
          , "SPO: \t"
              <> show (spoAcceptedRatio env gas)
              <> "\t"
              <> showAccepted (spoAccepted env st gas)
          , "DRep: \t"
              <> show (dRepAcceptedRatio env gasDRepVotes (gasAction gas))
              <> "\t"
              <> showAccepted (dRepAccepted env st gas)
          , "CC: \t"
              <> show (committeeAcceptedRatio members gasCommitteeVotes reCommitteeState reCurrentEpoch)
              <> "\t"
              <> showAccepted (committeeAccepted env st gas)
          , ""
          , "Spec extra info:"
          , either show T.unpack . runSpecTransM ctx $
              Agda.ratifyDebug
                <$> toSpecRep env
                <*> toSpecRep st
                <*> toSpecRep sig
          ]

  testConformance ctx env st@(RatifyState {rsEnactState}) sig@(RatifySignal actions) =
    labelRatios
      . property
      $ defaultTestConformance @fn @Conway @"RATIFY" ctx env st sig
    where
      bucket r
        | r == 0 % 1 = "=0%"
        | r <= 1 % 5 = "0%-20%"
        | r <= 2 % 5 = "20%-40%"
        | r <= 3 % 5 = "40%-60%"
        | r <= 4 % 5 = "60%-80%"
        | r < 1 % 1 = "80%-100%"
        | r == 1 % 1 = "=100%"
        | otherwise = error "ratio is not in the unit interval"
      committee = ensCommittee rsEnactState
      members = foldMap' (committeeMembers @Conway) committee
      ccBucket a =
        "CC yes votes ratio  \t"
          <> bucket
            ( committeeAcceptedRatio
                members
                (gasCommitteeVotes @Conway a)
                (reCommitteeState env)
                (reCurrentEpoch env)
            )
      drepBucket a =
        "DRep yes votes ratio\t"
          <> bucket
            (dRepAcceptedRatio env (gasDRepVotes a) (gasAction a))
      spoBucket a =
        "SPO yes votes ratio \t"
          <> bucket
            (spoAcceptedRatio env a)
      labelRatios
        | Just x <- SSeq.lookup 0 actions =
            label (ccBucket x)
              . label (drepBucket x)
              . label (spoBucket x)
        | otherwise = id

instance IsConwayUniv fn => ExecSpecRule fn "ENACT" Conway where
  type ExecEnvironment fn "ENACT" Conway = ConwayExecEnactEnv Conway
  type ExecState fn "ENACT" Conway = EnactState Conway
  type ExecSignal fn "ENACT" Conway = EnactSignal Conway

  environmentSpec _ = TrueSpec
  stateSpec _ _ = TrueSpec
  signalSpec _ _ _ = TrueSpec
  runAgdaRule env st sig =
    first (error "ENACT failed")
      . computationResultToEither
      $ Agda.enactStep env st sig

  classOf = Just . nameEnact

instance Inject (EpochExecEnv era) () where
  inject _ = ()

nameEnact :: EnactSignal era -> String
nameEnact (EnactSignal _ x) = nameGovAction x

nameGovAction :: GovAction era -> String
nameGovAction ParameterChange {} = "ParameterChange"
nameGovAction HardForkInitiation {} = "HardForkInitiation"
nameGovAction TreasuryWithdrawals {} = "TreasuryWithdrawals"
nameGovAction NoConfidence {} = "NoConfidence"
nameGovAction UpdateCommittee {} = "UpdateCommittee"
nameGovAction NewConstitution {} = "NewConstitution"
nameGovAction InfoAction {} = "InfoAction"

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "EPOCH" Conway
  where
  type ExecContext fn "EPOCH" Conway = [GovActionState Conway]
  type ExecEnvironment fn "EPOCH" Conway = EpochExecEnv Conway

  environmentSpec _ = epochEnvSpec

  stateSpec _ _ = epochStateSpec

  signalSpec _ _ _ = epochSignalSpec

  runAgdaRule env st sig =
    first (\case {})
      . computationResultToEither
      $ Agda.epochStep env st sig

  classOf = Just . nameEpoch

nameEpoch :: EpochNo -> String
nameEpoch x = show x

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "NEWEPOCH" Conway
  where
  type ExecContext fn "NEWEPOCH" Conway = [GovActionState Conway]
  type ExecEnvironment fn "NEWEPOCH" Conway = EpochExecEnv Conway

  environmentSpec _ = epochEnvSpec

  stateSpec _ _ = newEpochStateSpec

  signalSpec _ _ _ = epochSignalSpec

  runAgdaRule env st sig =
    first (\case {})
      . computationResultToEither
      $ Agda.newEpochStep env st sig
