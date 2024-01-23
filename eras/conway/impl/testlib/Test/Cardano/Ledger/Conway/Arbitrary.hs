{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Arbitrary (
  genUpdateCommittee,
  genNoConfidence,
  genTreasuryWithdrawals,
  genHardForkInitiation,
  genParameterChange,
  genNewConstitution,
  govActionGenerators,
  genConwayPlutusPurposePointer,
  genGovAction,
  genGovActionState,
  genPParamUpdateGovAction,
  genHardForkGovAction,
  genCommitteeGovAction,
  genConstitutionGovAction,
  ProposalsNewActions (..),
  ProposalsForEnactment (..),
  genProposals,
) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
  UpgradeConwayPParams (..),
 )
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.HKD (HKD, NoUpdate (..))
import Control.State.Transition.Extended (STS (Event))
import Data.Default.Class (def)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Word
import Lens.Micro
import Test.Cardano.Data (genNonEmptyMap)
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary (
  genValidAndUnknownCostModels,
  unFlexibleCostModels,
 )
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common

instance
  (Era era, Arbitrary (PParamsUpdate era)) =>
  Arbitrary (PulsingSnapshot era)
  where
  arbitrary = PulsingSnapshot <$> arbitrary <*> arbitrary <*> arbitrary

instance
  (Arbitrary (PParams era), Arbitrary (PParamsUpdate era), Era era) =>
  Arbitrary (DRepPulsingState era)
  where
  arbitrary = DRComplete <$> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (ConwayGenesis c) where
  arbitrary =
    ConwayGenesis
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (UpgradeConwayPParams Identity) where
  arbitrary =
    UpgradeConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (Delegatee c) where
  arbitrary =
    oneof
      [ DelegStake <$> arbitrary
      , DelegVote <$> arbitrary
      , DelegStakeVote <$> arbitrary <*> arbitrary
      ]

instance Crypto c => Arbitrary (ConwayDelegCert c) where
  arbitrary =
    oneof
      [ ConwayRegCert <$> arbitrary <*> arbitrary
      , ConwayUnRegCert <$> arbitrary <*> arbitrary
      , ConwayDelegCert <$> arbitrary <*> arbitrary
      , ConwayRegDelegCert <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Era era => Arbitrary (ConwayTxCert era) where
  arbitrary =
    oneof
      [ ConwayTxCertDeleg <$> arbitrary
      , ConwayTxCertPool <$> arbitrary
      , ConwayTxCertGov <$> arbitrary
      ]

instance Crypto c => Arbitrary (ConwayGovCert c) where
  arbitrary =
    oneof
      [ ConwayRegDRep <$> arbitrary <*> arbitrary <*> arbitrary
      , ConwayUnRegDRep <$> arbitrary <*> arbitrary
      , ConwayAuthCommitteeHotKey <$> arbitrary <*> arbitrary
      , ConwayResignCommitteeColdKey <$> arbitrary <*> arbitrary
      ]

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (ConwayGovState era)
  where
  arbitrary =
    ConwayGovState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (RatifyState era)
  where
  arbitrary =
    RatifyState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (RatifyEnv era)
  where
  arbitrary =
    RatifyEnv
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  (Era era, Arbitrary (PParams era), Arbitrary (PParamsUpdate era)) =>
  Arbitrary (EnactState era)
  where
  arbitrary =
    EnactState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

_uniqueIdGovActions ::
  (Era era, Arbitrary (PParamsUpdate era)) =>
  Gen (SSeq.StrictSeq (GovActionState era))
_uniqueIdGovActions = SSeq.fromList . nubBy (\x y -> gasId x == gasId y) <$> arbitrary

instance Era era => Arbitrary (PrevGovActionIds era) where
  arbitrary =
    fmap PrevGovActionIds $
      PForest
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

data ProposalsForEnactment era
  = ProposalsForEnactment
      (Proposals era)
      (Seq.Seq (GovActionId (EraCrypto era)))
      (Set.Set (GovActionId (EraCrypto era)))
  deriving (Show, Eq)

instance
  (Era era, Arbitrary (PParamsUpdate era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (ProposalsForEnactment era)
  where
  arbitrary = do
    ps <- genProposals @era (2, 50)
    pparamUpdates <- chooseLineage pfPParamUpdateL ps Seq.Empty
    hardForks <- chooseLineage pfHardForkL ps Seq.Empty
    committees <- chooseLineage pfCommitteeL ps Seq.Empty
    constitutions <- chooseLineage pfConstitutionL ps Seq.Empty
    sequencedGais <-
      sequenceLineages
        ( Seq.filter
            (not . Seq.null)
            (Seq.fromList [pparamUpdates, hardForks, committees, constitutions])
        )
        Seq.Empty
    let expiredGais =
          Set.fromList (toList $ proposalsIds ps)
            `Set.difference` Set.fromList (toList sequencedGais)
    pure $ ProposalsForEnactment ps sequencedGais expiredGais
    where
      chooseLineage ::
        (forall f. Lens' (PForest f era) (f (GovPurposeId p era))) ->
        Proposals era ->
        Seq.Seq (GovActionId (EraCrypto era)) ->
        Gen (Seq.Seq (GovActionId (EraCrypto era)))
      chooseLineage forestL ps = \case
        Seq.Empty ->
          let children = ps ^. pRootsL . forestL . prChildrenL
           in if Set.null children
                then pure Seq.Empty
                else do
                  child <- elements $ toList children
                  chooseLineage forestL ps (Seq.Empty Seq.:|> unGovPurposeId child)
        lineage@(_ Seq.:|> gai) ->
          let children = ps ^. pGraphL . forestL . pGraphNodesL . to (Map.! GovPurposeId gai) . peChildrenL
           in if Set.null children
                then pure lineage
                else do
                  child <- elements $ toList children
                  chooseLineage forestL ps (lineage Seq.:|> unGovPurposeId child)
      consumeHeadAtIndex :: Int -> Seq.Seq (Seq.Seq a) -> (a, Seq.Seq (Seq.Seq a))
      consumeHeadAtIndex idx ss = (ss `Seq.index` idx `Seq.index` 0, Seq.adjust' (Seq.drop 1) idx ss)
      sequenceLineages ::
        Seq.Seq (Seq.Seq (GovActionId (EraCrypto era))) ->
        Seq.Seq (GovActionId (EraCrypto era)) ->
        Gen (Seq.Seq (GovActionId (EraCrypto era)))
      sequenceLineages lineages sequenced = case lineages of
        Seq.Empty -> pure sequenced
        _ -> do
          index <- chooseInt (0, length lineages - 1)
          let (chosen, adjustedLineages) = consumeHeadAtIndex index lineages
          sequenceLineages (Seq.filter (not . Seq.null) adjustedLineages) (sequenced Seq.:|> chosen)

data ProposalsNewActions era = ProposalsNewActions (Proposals era) [GovActionState era]
  deriving (Show, Eq)

instance
  (Era era, Arbitrary (PParamsUpdate era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (ProposalsNewActions era)
  where
  arbitrary = do
    ps <- arbitrary
    i <- chooseInt (2, 20)
    gass <- vectorOf i $ genGovActionState =<< genGovAction ps
    pure $ ProposalsNewActions ps gass

instance
  (Era era, Arbitrary (PParamsUpdate era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (Proposals era)
  where
  arbitrary = genProposals (2, 30)

genProposals ::
  forall era.
  ( Era era
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  (Int, Int) ->
  Gen (Proposals era)
genProposals range = do
  pgais <- arbitrary
  i <- chooseInt range
  go (def & pRootsL .~ fromPrevGovActionIds pgais) i
  where
    go :: Proposals era -> Int -> Gen (Proposals era)
    go ps n
      | n <= 0 = pure ps
      | otherwise = do
          gas <- genGovActionState @era =<< genGovAction ps
          case proposalsAddAction gas ps of
            Nothing -> error "Error adding GovActionState to Proposals"
            Just ps' -> go ps' (n - 1)

genGovAction ::
  forall era.
  (Era era, Arbitrary (PParamsHKD StrictMaybe era)) =>
  Proposals era ->
  Gen (GovAction era)
genGovAction ps =
  oneof
    [ genWithParent genPParamUpdateGovAction pfPParamUpdateL
    , genWithParent genHardForkGovAction pfHardForkL
    , genWithParent genCommitteeGovAction pfCommitteeL
    , genWithParent genConstitutionGovAction pfConstitutionL
    ]
  where
    genWithParent ::
      (StrictMaybe (GovPurposeId p era) -> Gen (GovAction era)) ->
      (forall f. Lens' (PForest f era) (f (GovPurposeId p era))) ->
      Gen (GovAction era)
    genWithParent gen forestL =
      gen
        =<< elements
          ( (ps ^. pRootsL . forestL . prRootL)
              : fmap SJust (Map.keys $ ps ^. pGraphL . forestL . pGraphNodesL)
          )

genPParamUpdateGovAction ::
  ( Era era
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  StrictMaybe (GovPurposeId 'PParamUpdatePurpose era) ->
  Gen (GovAction era)
genPParamUpdateGovAction parent = ParameterChange parent <$> arbitrary <*> arbitrary

genHardForkGovAction ::
  StrictMaybe (GovPurposeId 'HardForkPurpose era) ->
  Gen (GovAction era)
genHardForkGovAction parent = HardForkInitiation parent <$> arbitrary

genCommitteeGovAction ::
  Era era =>
  StrictMaybe (GovPurposeId 'CommitteePurpose era) ->
  Gen (GovAction era)
genCommitteeGovAction parent =
  oneof
    [ pure $ NoConfidence parent
    , UpdateCommittee parent <$> arbitrary <*> arbitrary <*> arbitrary
    ]

genConstitutionGovAction ::
  Era era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  Gen (GovAction era)
genConstitutionGovAction parent = NewConstitution parent <$> arbitrary

genGovActionState :: Era era => GovAction era -> Gen (GovActionState era)
genGovActionState ga =
  GovActionState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure ga
    <*> arbitrary
    <*> arbitrary

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovActionState era) where
  arbitrary = genGovActionState =<< arbitrary

genParameterChange :: (Era era, Arbitrary (PParamsUpdate era)) => Gen (GovAction era)
genParameterChange = ParameterChange <$> arbitrary <*> arbitrary <*> arbitrary

genHardForkInitiation :: Era era => Gen (GovAction era)
genHardForkInitiation = HardForkInitiation <$> arbitrary <*> arbitrary

genTreasuryWithdrawals :: Era era => Gen (GovAction era)
genTreasuryWithdrawals = TreasuryWithdrawals <$> arbitrary <*> arbitrary

genNoConfidence :: Era era => Gen (GovAction era)
genNoConfidence = NoConfidence <$> arbitrary

genUpdateCommittee :: Era era => Gen (GovAction era)
genUpdateCommittee =
  UpdateCommittee
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

genNewConstitution :: Era era => Gen (GovAction era)
genNewConstitution = NewConstitution <$> arbitrary <*> arbitrary

govActionGenerators ::
  ( Era era
  , Arbitrary (PParamsUpdate era)
  ) =>
  [Gen (GovAction era)]
govActionGenerators =
  [ genParameterChange
  , genHardForkInitiation
  , genTreasuryWithdrawals
  , genNoConfidence
  , genUpdateCommittee
  , genNewConstitution
  , pure InfoAction
  ]

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovAction era) where
  arbitrary = oneof govActionGenerators

instance Era era => Arbitrary (Committee era) where
  arbitrary = Committee <$> arbitrary <*> arbitrary

instance Crypto c => Arbitrary (GovActionId c) where
  arbitrary = GovActionId <$> arbitrary <*> arbitrary

deriving instance Arbitrary GovActionIx

deriving instance
  forall (p :: GovActionPurpose) era.
  Crypto (EraCrypto era) =>
  Arbitrary (GovPurposeId p era)

instance Crypto c => Arbitrary (Voter c) where
  arbitrary =
    oneof
      [ CommitteeVoter <$> arbitrary
      , DRepVoter <$> arbitrary
      , StakePoolVoter <$> arbitrary
      ]

instance Arbitrary Vote where
  arbitrary = arbitraryBoundedEnum

instance
  ( ConwayEraTxBody era
  , Arbitrary (Sized (TxOut era))
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , Arbitrary (PParamsUpdate era)
  ) =>
  Arbitrary (ConwayTxBody era)
  where
  arbitrary =
    ConwayTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( Era era
  , Arbitrary (TxCert era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (ConwayPlutusPurpose AsItem era)
  where
  arbitrary =
    oneof
      [ ConwaySpending <$> arbitrary
      , ConwayMinting <$> arbitrary
      , ConwayCertifying <$> arbitrary
      , ConwayRewarding <$> arbitrary
      , ConwayVoting <$> arbitrary
      , ConwayProposing <$> arbitrary
      ]

instance
  Era era =>
  Arbitrary (ConwayPlutusPurpose AsIndex era)
  where
  arbitrary = arbitrary >>= genConwayPlutusPurposePointer

genConwayPlutusPurposePointer :: Word32 -> Gen (ConwayPlutusPurpose AsIndex era)
genConwayPlutusPurposePointer i =
  elements
    [ ConwaySpending (AsIndex i)
    , ConwayMinting (AsIndex i)
    , ConwayCertifying (AsIndex i)
    , ConwayRewarding (AsIndex i)
    , ConwayVoting (AsIndex i)
    , ConwayProposing (AsIndex i)
    ]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Conway.Rules -----------------------------------------------------------
------------------------------------------------------------------------------------------

-- GOV

instance (Era era, Arbitrary (PParamsHKD Identity era)) => Arbitrary (GovEnv era) where
  arbitrary =
    GovEnv
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Era era => Arbitrary (VotingProcedure era) where
  arbitrary = VotingProcedure <$> arbitrary <*> arbitrary

instance Era era => Arbitrary (VotingProcedures era) where
  arbitrary = VotingProcedures <$> liftArbitrary (genNonEmptyMap arbitrary arbitrary)

instance (Era era, Arbitrary (PParamsUpdate era)) => Arbitrary (ProposalProcedure era) where
  arbitrary =
    ProposalProcedure
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (EraPParams era, Arbitrary (PParamsUpdate era)) => Arbitrary (GovProcedures era) where
  arbitrary =
    GovProcedures <$> arbitrary <*> arbitrary

instance Era era => Arbitrary (ConwayGovPredFailure era) where
  arbitrary = GovActionsDoNotExist <$> arbitrary

instance
  ( Arbitrary (PredicateFailure (EraRule "UTXOW" era))
  , Arbitrary (PredicateFailure (EraRule "CERTS" era))
  , Arbitrary (PredicateFailure (EraRule "GOV" era))
  ) =>
  Arbitrary (ConwayLedgerPredFailure era)
  where
  arbitrary =
    oneof
      [ ConwayUtxowFailure <$> arbitrary
      , ConwayCertsFailure <$> arbitrary
      , ConwayGovFailure <$> arbitrary
      ]

-- EPOCH

instance
  ( Era era
  , Arbitrary (Event (EraRule "POOLREAP" era))
  , Arbitrary (Event (EraRule "SNAP" era))
  ) =>
  Arbitrary (ConwayEpochEvent era)
  where
  arbitrary =
    oneof
      [ PoolReapEvent <$> arbitrary
      , SnapEvent <$> arbitrary
      ]

-- NEWEPOCH

instance
  ( Era era
  , Arbitrary (Event (EraRule "RUPD" era))
  ) =>
  Arbitrary (ConwayNewEpochEvent era)
  where
  arbitrary =
    oneof
      [ DeltaRewardEvent <$> arbitrary
      , RestrainedRewards <$> arbitrary <*> arbitrary <*> arbitrary
      ]

-- CERTS

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "CERT" era))
  ) =>
  Arbitrary (ConwayCertsPredFailure era)
  where
  arbitrary =
    oneof
      [ DelegateeNotRegisteredDELEG <$> arbitrary
      , WithdrawalsNotInRewardsCERTS <$> arbitrary
      , CertFailure <$> arbitrary
      ]

-- CERT

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "DELEG" era))
  , Arbitrary (PredicateFailure (EraRule "POOL" era))
  , Arbitrary (PredicateFailure (EraRule "GOVCERT" era))
  ) =>
  Arbitrary (ConwayCertPredFailure era)
  where
  arbitrary =
    oneof
      [ DelegFailure <$> arbitrary
      , PoolFailure <$> arbitrary
      , GovCertFailure <$> arbitrary
      ]

-- DELEG

instance
  Era era =>
  Arbitrary (ConwayDelegPredFailure era)
  where
  arbitrary =
    oneof
      [ IncorrectDepositDELEG <$> arbitrary
      , StakeKeyRegisteredDELEG <$> arbitrary
      , StakeKeyNotRegisteredDELEG <$> arbitrary
      , StakeKeyHasNonZeroRewardAccountBalanceDELEG <$> arbitrary
      , DRepAlreadyRegisteredForStakeKeyDELEG <$> arbitrary
      ]

-- GOVCERT

instance Era era => Arbitrary (ConwayGovCertPredFailure era) where
  arbitrary =
    oneof
      [ ConwayDRepAlreadyRegistered <$> arbitrary
      , ConwayDRepNotRegistered <$> arbitrary
      , ConwayDRepIncorrectDeposit <$> arbitrary <*> arbitrary
      , ConwayCommitteeHasPreviouslyResigned <$> arbitrary
      ]

instance Arbitrary (HKD f a) => Arbitrary (THKD t f a) where
  arbitrary = THKD <$> arbitrary

instance Era era => Arbitrary (ConwayPParams Identity era) where
  arbitrary =
    ConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (THKD . unFlexibleCostModels <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Era era => Arbitrary (ConwayPParams StrictMaybe era) where
  arbitrary =
    ConwayPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure NoUpdate
      <*> arbitrary
      <*> arbitrary
      <*> (THKD <$> oneof [SJust <$> genValidAndUnknownCostModels, pure SNothing])
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary PoolVotingThresholds where
  arbitrary =
    PoolVotingThresholds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary DRepVotingThresholds where
  arbitrary =
    DRepVotingThresholds
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
