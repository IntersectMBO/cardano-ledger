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
  ConwayCertExecContext (..),
  ConwayRatifyExecContext (..),
  nameEpoch,
  nameEnact,
  nameGovAction,
  crecTreasuryL,
  crecGovActionMapL,
) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Inject (..),
  Network,
  StrictMaybe (..),
  addEpochInterval,
  natVersion, ProtVer (..),
 )
import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  CommitteeState (..),
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (Era (..), EraPParams (..), PParams, ppMaxTxSizeL)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  VotingProcedures,
  gasAction,
  showGovActionType,
 )
import Cardano.Ledger.Conway.Rules (
  EnactSignal (..),
  acceptedByEveryone,
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  spoAccepted,
  spoAcceptedRatio,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Constrained
import Constrained.Base (fromList_)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (&), (.~))
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
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (ConwayExecEnactEnv (..))
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
  UtxoExecContext (..),
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common hiding (arbitrary, forAll, prop, var)
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import Test.Cardano.Ledger.Generic.GenState (runGenRS, GenEnv (..), GenState (..))
import qualified Test.Cardano.Ledger.Generic.GenState as GenSize

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "UTXO" Conway
  where
  type ExecContext fn "UTXO" Conway = UtxoExecContext Conway

  genExecContext = do
    let proof = Proof.reify @Conway
    uecSlotNo <- arbitrary
    ((uecUTxO, uecTx), gs) <- runGenRS proof GenSize.small $
      genAlonzoTx proof uecSlotNo
    let
      uecPParams = gePParams (gsGenEnv gs)
        & ppMaxTxSizeL .~ 3000
        & ppProtocolVersionL .~ ProtVer (natVersion @10) 0
    pure UtxoExecContext {..}

  environmentSpec = utxoEnvSpec

  stateSpec = utxoStateSpec

  signalSpec ctx _ _ = utxoTxSpec ctx

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.utxoStep env st sig

data ConwayCertExecContext era = ConwayCertExecContext
  { ccecWithdrawals :: !(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  , ccecVotes :: !(VotingProcedures era)
  }
  deriving (Generic, Eq, Show)

instance Era era => Arbitrary (ConwayCertExecContext era) where
  arbitrary =
    ConwayCertExecContext
      <$> arbitrary
      <*> arbitrary

instance Era era => EncCBOR (ConwayCertExecContext era) where
  encCBOR x@(ConwayCertExecContext _ _) =
    let ConwayCertExecContext {..} = x
     in encode $
          Rec ConwayCertExecContext
            !> To ccecWithdrawals
            !> To ccecVotes

instance Era era => DecCBOR (ConwayCertExecContext era) where
  decCBOR =
    decode $
      RecD ConwayCertExecContext
        <! From
        <! From

instance
  c ~ EraCrypto era =>
  Inject
    (ConwayCertExecContext era)
    (Map (Network, Credential 'Staking c) Coin)
  where
  inject = ccecWithdrawals

instance Inject (ConwayCertExecContext era) (VotingProcedures era) where
  inject = ccecVotes

instance Era era => ToExpr (ConwayCertExecContext era)

instance Era era => NFData (ConwayCertExecContext era)

data ConwayRatifyExecContext era = ConwayRatifyExecContext
  { crecTreasury :: Coin
  , crecGovActionMap :: [GovActionState era]
  }
  deriving (Generic, Eq, Show)

crecTreasuryL :: Lens' (ConwayRatifyExecContext era) Coin
crecTreasuryL = lens crecTreasury (\x y -> x {crecTreasury = y})

crecGovActionMapL :: Lens' (ConwayRatifyExecContext era) [GovActionState era]
crecGovActionMapL = lens crecGovActionMap (\x y -> x {crecGovActionMap = y})

instance EraPParams era => EncCBOR (ConwayRatifyExecContext era) where
  encCBOR x@(ConwayRatifyExecContext _ _) =
    let ConwayRatifyExecContext {..} = x
     in encode $
          Rec ConwayRatifyExecContext
            !> To crecTreasury
            !> To crecGovActionMap

instance EraPParams era => DecCBOR (ConwayRatifyExecContext era) where
  decCBOR =
    decode $
      RecD ConwayRatifyExecContext
        <! From
        <! From

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
          , preferSmallerCCMinSizeValues pp
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
        assert $ major ==. lit (natVersion @10)
    preferSmallerCCMinSizeValues ::
      IsConwayUniv fn =>
      Term fn (PParams Conway) ->
      Pred fn
    preferSmallerCCMinSizeValues pp = match pp $ \pp' ->
      match (sel @24 pp') $ \ccMinSize ->
        satisfies ccMinSize $
          chooseSpec
            (1, TrueSpec)
            (1, constrained (<=. committeeSize))
      where
        committeeSize = lit . fromIntegral . Set.size $ ccColdKeys

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
    unlines $ specExtraInfo : (actionAcceptedRatio <$> toList actions)
    where
      members = foldMap' (committeeMembers @Conway) $ ensCommittee (rsEnactState st)
      showAccepted True = "✓"
      showAccepted False = "×"
      specExtraInfo =
        unlines
          [ "Spec extra info:"
          , either show T.unpack . runSpecTransM ctx $
              Agda.ratifyDebug
                <$> toSpecRep env
                <*> toSpecRep st
                <*> toSpecRep sig
          , ""
          ]
      actionAcceptedRatio gas@GovActionState {..} =
        unlines
          [ "GovActionId: \t" <> showExpr gasId
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
          ]

  testConformance ctx env st@(RatifyState {rsEnactState}) sig@(RatifySignal actions) =
    labelRatios $
      defaultTestConformance @fn @Conway @"RATIFY" ctx env st sig
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
      acceptedActions = fmap gasAction . filter (acceptedByEveryone env st) $ toList actions
      acceptedActionTypes = Set.fromList $ showGovActionType <$> acceptedActions
      labelRatios
        | Just x <- SSeq.lookup 0 actions =
            label (ccBucket x)
              . label (drepBucket x)
              . label (spoBucket x)
              . foldr'
                (\a f -> label ("Accepted at least one " <> a) . f)
                id
                (toList acceptedActionTypes)
        | otherwise = id

newtype ConwayEnactExecContext era = ConwayEnactExecContext
  { ceecMaxTerm :: EpochInterval
  }
  deriving (Generic)

instance Arbitrary (ConwayEnactExecContext era) where
  arbitrary = ConwayEnactExecContext <$> arbitrary

instance NFData (ConwayEnactExecContext era)

instance ToExpr (ConwayEnactExecContext era)

instance Era era => EncCBOR (ConwayEnactExecContext era) where
  encCBOR (ConwayEnactExecContext x) = encCBOR x

enactSignalSpec ::
  IsConwayUniv fn =>
  ConwayEnactExecContext Conway ->
  ConwayExecEnactEnv Conway ->
  EnactState Conway ->
  Specification fn (EnactSignal Conway)
enactSignalSpec ConwayEnactExecContext {..} ConwayExecEnactEnv {..} EnactState {..} =
  constrained' $ \gid action ->
    [ assert $ gid ==. lit ceeeGid
    , -- TODO get rid of this by modifying the spec so that ENACT can't fail.
      -- Right now this constraint makes the generator avoid cases where
      -- the spec would fail, because such proposals would be handled in RATIFY
      -- and wouldn't make it to ENACT.
      (caseOn action)
        (branch $ \_ _ _ -> True)
        (branch $ \_ _ -> True)
        ( branch $ \newWdrls _ ->
            sum_ (rng_ newWdrls) + lit (sum ensWithdrawals) <=. lit ceeeTreasury
        )
        (branch $ \_ -> True)
        ( branch $ \_ _ newMembers _ ->
            let maxTerm = addEpochInterval ceeeEpoch ceecMaxTerm
             in forAll (rng_ newMembers) (<=. lit maxTerm)
        )
        (branch $ \_ _ -> True)
        (branch $ \_ -> True)
    ]

enactStateSpec ::
  IsConwayUniv fn =>
  ConwayEnactExecContext Conway ->
  ConwayExecEnactEnv Conway ->
  Specification fn (EnactState Conway)
enactStateSpec ConwayEnactExecContext {..} ConwayExecEnactEnv {..} =
  constrained' $ \_ _ curPParams _ treasury wdrls _ ->
    [ match curPParams $ \pp -> match (sel @25 pp) (==. lit ceecMaxTerm)
    , assert $ sum_ (rng_ wdrls) <=. treasury
    , assert $ treasury ==. lit ceeeTreasury
    ]

instance IsConwayUniv fn => ExecSpecRule fn "ENACT" Conway where
  type ExecContext fn "ENACT" Conway = ConwayEnactExecContext Conway
  type ExecEnvironment fn "ENACT" Conway = ConwayExecEnactEnv Conway
  type ExecState fn "ENACT" Conway = EnactState Conway
  type ExecSignal fn "ENACT" Conway = EnactSignal Conway

  environmentSpec _ = TrueSpec
  stateSpec = enactStateSpec
  signalSpec = enactSignalSpec
  runAgdaRule env st sig =
    first (\e -> error $ "ENACT failed with:\n" <> show e)
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
