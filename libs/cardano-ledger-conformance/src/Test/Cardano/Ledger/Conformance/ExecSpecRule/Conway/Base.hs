{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  conwayCertExecContextSpec,
  ConwayRatifyExecContext (..),
  nameEpoch,
  nameEnact,
  nameGovAction,
  crecTreasuryL,
  crecGovActionMapL,
  enactStateSpec,
  externalFunctions,
) where

import Cardano.Crypto.DSIGN (SignedDSIGN (..), verifySignedDSIGN)
import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Inject (..),
  Network (..),
  StrictMaybe (..),
  addEpochInterval,
  natVersion,
 )
import Cardano.Ledger.Binary (DecCBOR (decCBOR), EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Coders (Decode (From, RecD), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  CommitteeState (..),
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (Era (..), EraPParams (..), PParams)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionState (..),
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  VotingProcedures,
  ensProtVerL,
  gasAction,
  rsEnactStateL,
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
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..))
import Constrained hiding (inject)
import Data.Either (isRight)
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Sequence.Strict as SSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import qualified Lib as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Binary.TreeDiff (tableDoc)
import Test.Cardano.Ledger.Common (Arbitrary (..))
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  integerToHash,
  runSpecTransM,
  unComputationResult,
  unComputationResult_,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (defaultTestConformance)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (vkeyFromInteger)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  ConwayExecEnactEnv (..),
  DepositPurpose,
  signatureFromInteger,
 )
import Test.Cardano.Ledger.Constrained.Conway (
  delegateeSpec,
  newEpochStateSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.Epoch
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.Instances.PParams (
  committeeMaxTermLength_,
  committeeMinSize_,
  protocolVersion_,
 )

import Cardano.Crypto.Hash (ByteString, Hash)
import Cardano.Ledger.Crypto (DSIGN, HASH)
import Cardano.Ledger.Keys (KeyRole (..), VKey (..))
import Test.Cardano.Ledger.Constrained.Conway.Utxo (witnessDepositPurpose)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse (WitUniv (..), witness)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (Reflect)
import Test.Cardano.Ledger.Imp.Common hiding (arbitrary, forAll, prop, var, witness)

-- ================================================================

data ConwayCertExecContext era
  = -- | The UMap of the DState has a field with type: Map (Credential 'Staking) DRep
    --   The VState field vsDReps has type: Map (Credential DRepRole) DRepState
    --   The DRepState field drepDelegs has type: Set (Credential Staking)
    --   Every (Credential 'DRepRole c) corresponds to a unique (DRep)
    -- the ccecDelegatees field helps maintain that correspondance, It is used in
    -- vstateSpec and bootstrapDStateSpec. Also see
    -- getDelegatees :: DState era -> Map (Credential 'DRepRole) (Set (Credential 'Staking))
    -- in Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs, which defines the exact correspondance.  }
    ConwayCertExecContext
    { ccecWithdrawals :: !(Map RewardAccount Coin)
    , ccecDeposits :: !(Map DepositPurpose Coin)
    , ccecVotes :: !(VotingProcedures era)
    , ccecDelegatees :: !(Set (Credential 'DRepRole))
    }
  deriving (Generic, Eq, Show)

instance HasSimpleRep (ConwayCertExecContext era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ConwayCertExecContext era)

-- No particular constraints, other than witnessing
conwayCertExecContextSpec ::
  forall fn era.
  (Reflect era, IsConwayUniv fn) =>
  WitUniv era -> Integer -> Specification fn (ConwayCertExecContext era)
conwayCertExecContextSpec univ wdrlsize = constrained $ \ [var|ccec|] ->
  match ccec $ \ [var|withdrawals|] [var|deposits|] _ [var|delegatees|] ->
    [ assert $
        [ witness univ (dom_ withdrawals)
        , assert $ sizeOf_ (dom_ withdrawals) <=. (lit wdrlsize)
        ]
    , forAll (dom_ deposits) $ \dp -> satisfies dp (witnessDepositPurpose univ)
    , satisfies delegatees (delegateeSpec @fn @era univ)
    ]

instance Reflect era => Arbitrary (ConwayCertExecContext era) where
  arbitrary =
    ConwayCertExecContext
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Era era => EncCBOR (ConwayCertExecContext era) where
  encCBOR x@(ConwayCertExecContext _ _ _ _) =
    let ConwayCertExecContext {..} = x
     in encode $
          Rec ConwayCertExecContext
            !> To ccecWithdrawals
            !> To ccecDeposits
            !> To ccecVotes
            !> To ccecDelegatees

instance Reflect era => DecCBOR (ConwayCertExecContext era) where
  decCBOR =
    decode $
      RecD ConwayCertExecContext
        <! From
        <! From
        <! From
        <! From

instance Inject (ConwayCertExecContext era) (Map RewardAccount Coin) where
  inject = ccecWithdrawals

instance Inject (ConwayCertExecContext era) (VotingProcedures era) where
  inject = ccecVotes

instance Inject (ConwayCertExecContext era) (Map DepositPurpose Coin) where
  inject = ccecDeposits

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
  , HasSpec fn (RatifyEnv ConwayEra)
  , HasSpec fn (SimpleRep (RatifyEnv ConwayEra))
  ) =>
  ConwayRatifyExecContext ConwayEra ->
  Specification fn (RatifyEnv ConwayEra)
ratifyEnvSpec ConwayRatifyExecContext {crecGovActionMap} =
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
  ConwayRatifyExecContext ConwayEra ->
  RatifyEnv ConwayEra ->
  Specification fn (RatifyState ConwayEra)
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
    disableBootstrap :: IsConwayUniv fn => Term fn (PParams ConwayEra) -> Pred fn
    disableBootstrap pp = match pp $ \simplepp ->
      match (protocolVersion_ simplepp) $ \major _ ->
        assert $ not_ (major ==. lit (natVersion @9))

    preferSmallerCCMinSizeValues ::
      IsConwayUniv fn =>
      Term fn (PParams ConwayEra) ->
      Pred fn
    preferSmallerCCMinSizeValues pp = match pp $ \simplepp ->
      satisfies (committeeMinSize_ simplepp) $
        chooseSpec
          (1, TrueSpec)
          (1, constrained (<=. committeeSize))
      where
        committeeSize = lit . fromIntegral . Set.size $ ccColdKeys

ratifySignalSpec ::
  IsConwayUniv fn =>
  ConwayRatifyExecContext ConwayEra ->
  Specification fn (RatifySignal ConwayEra)
ratifySignalSpec ConwayRatifyExecContext {crecGovActionMap} =
  constrained $ \sig ->
    match sig $ \gasS ->
      match gasS $ \gasL ->
        forAll gasL $ \gas ->
          gas `elem_` lit crecGovActionMap

instance IsConwayUniv fn => ExecSpecRule fn "RATIFY" ConwayEra where
  type ExecContext fn "RATIFY" ConwayEra = ConwayRatifyExecContext ConwayEra

  genExecContext = arbitrary

  environmentSpec = ratifyEnvSpec

  stateSpec = ratifyStateSpec

  signalSpec ctx _env _st = ratifySignalSpec ctx

  runAgdaRule env st sig = unComputationResult_ $ Agda.ratifyStep env st sig

  extraInfo _ ctx env@RatifyEnv {..} st sig@(RatifySignal actions) _ =
    PP.vsep $ specExtraInfo : (actionAcceptedRatio <$> toList actions)
    where
      members = foldMap' (committeeMembers @ConwayEra) $ ensCommittee (rsEnactState st)
      showAccepted True = PP.brackets "✓"
      showAccepted False = PP.brackets "×"
      showRatio r = PP.viaShow (numerator r) <> "/" <> PP.viaShow (denominator r)
      specExtraInfo =
        PP.vsep
          [ "Spec extra info:"
          , either PP.viaShow PP.pretty . runSpecTransM ctx $
              Agda.ratifyDebug
                <$> toSpecRep env
                <*> toSpecRep st
                <*> toSpecRep sig
          ]
      pv = st ^. rsEnactStateL . ensProtVerL
      actionAcceptedRatio gas@GovActionState {..} =
        tableDoc
          (Just "GovActionState")
          [
            ( "GovActionId:"
            , PP.line <> PP.indent 2 (ansiExpr gasId)
            )
          ,
            ( "SPO:"
            , showAccepted (spoAccepted env st gas)
                PP.<+> showRatio (spoAcceptedRatio env gas pv)
            )
          ,
            ( "DRep:"
            , showAccepted (dRepAccepted env st gas)
                PP.<+> showRatio (dRepAcceptedRatio env gasDRepVotes (gasAction gas))
            )
          ,
            ( "CC:"
            , showAccepted (committeeAccepted env st gas)
                PP.<+> showRatio (committeeAcceptedRatio members gasCommitteeVotes reCommitteeState reCurrentEpoch)
            )
          ]

  testConformance ctx env st@(RatifyState {rsEnactState}) sig@(RatifySignal actions) =
    labelRatios $
      defaultTestConformance @fn @ConwayEra @"RATIFY" ctx env st sig
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
      members = foldMap' (committeeMembers @ConwayEra) committee
      pv = st ^. rsEnactStateL . ensProtVerL
      ccBucket a =
        "CC yes votes ratio  \t"
          <> bucket
            ( committeeAcceptedRatio
                members
                (gasCommitteeVotes @ConwayEra a)
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
            (spoAcceptedRatio env a pv)
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
  ConwayEnactExecContext ConwayEra ->
  ConwayExecEnactEnv ConwayEra ->
  EnactState ConwayEra ->
  Specification fn (EnactSignal ConwayEra)
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
            [ assert $ sum_ (rng_ newWdrls) + lit (sum ensWithdrawals) <=. lit ceeeTreasury
            , assert $ forAll' newWdrls $ \acct _ ->
                match acct $ \network _ ->
                  network ==. lit Testnet
            ]
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
  ConwayEnactExecContext ConwayEra ->
  ConwayExecEnactEnv ConwayEra ->
  Specification fn (EnactState ConwayEra)
enactStateSpec ConwayEnactExecContext {..} ConwayExecEnactEnv {..} =
  constrained' $ \_ _ curPParams _ treasury wdrls _ ->
    [ match curPParams $ \simplepp -> committeeMaxTermLength_ simplepp ==. lit ceecMaxTerm
    , assert $ sum_ (rng_ wdrls) <=. treasury
    , assert $ treasury ==. lit ceeeTreasury
    ]

instance IsConwayUniv fn => ExecSpecRule fn "ENACT" ConwayEra where
  type ExecContext fn "ENACT" ConwayEra = ConwayEnactExecContext ConwayEra
  type ExecEnvironment fn "ENACT" ConwayEra = ConwayExecEnactEnv ConwayEra
  type ExecState fn "ENACT" ConwayEra = EnactState ConwayEra
  type ExecSignal fn "ENACT" ConwayEra = EnactSignal ConwayEra

  environmentSpec _ = TrueSpec
  stateSpec = enactStateSpec
  signalSpec = enactSignalSpec
  runAgdaRule env st sig = unComputationResult $ Agda.enactStep env st sig

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

-- The `fn ~ ConwayFn` thing here is because `ConwayFn` is a type alias
-- and those shouldn't go in instance heads apparently.
instance fn ~ ConwayFn => ExecSpecRule fn "EPOCH" ConwayEra where
  type ExecContext fn "EPOCH" ConwayEra = [GovActionState ConwayEra]
  type ExecEnvironment fn "EPOCH" ConwayEra = EpochExecEnv ConwayEra

  environmentSpec _ = epochEnvSpec

  stateSpec _ = epochStateSpec . lit . eeeEpochNo

  signalSpec _ env _ = epochSignalSpec (eeeEpochNo env)

  runAgdaRule env st sig = unComputationResult_ $ Agda.epochStep env st sig

  classOf = Just . nameEpoch

nameEpoch :: EpochNo -> String
nameEpoch x = show x

-- The `fn ~ ConwayFn` thing here is because `ConwayFn` is a type alias
-- and those shouldn't go in instance heads apparently.
instance fn ~ ConwayFn => ExecSpecRule fn "NEWEPOCH" ConwayEra where
  type ExecContext fn "NEWEPOCH" ConwayEra = [GovActionState ConwayEra]
  type ExecEnvironment fn "NEWEPOCH" ConwayEra = EpochExecEnv ConwayEra

  environmentSpec _ = epochEnvSpec

  stateSpec _ _ = newEpochStateSpec

  signalSpec _ env _ = epochSignalSpec (eeeEpochNo env)

  runAgdaRule env st sig = unComputationResult_ $ Agda.newEpochStep env st sig

externalFunctions :: Agda.ExternalFunctions
externalFunctions = Agda.MkExternalFunctions {..}
  where
    extIsSigned vk ser sig =
      isRight $
        verifySignedDSIGN
          @DSIGN
          @(Hash HASH ByteString)
          ()
          vkey
          hash
          signature
      where
        vkey =
          unVKey
            . fromMaybe (error "Failed to convert an Agda VKey to a Haskell VKey")
            $ vkeyFromInteger vk
        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser
        signature =
          SignedDSIGN
            . fromMaybe
              (error "Failed to decode the signature")
            $ signatureFromInteger sig
