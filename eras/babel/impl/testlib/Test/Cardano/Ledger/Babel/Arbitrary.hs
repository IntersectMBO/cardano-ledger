{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babel.Arbitrary (
  genUpdateCommittee,
  genNoConfidence,
  genTreasuryWithdrawals,
  genHardForkInitiation,
  genParameterChange,
  genNewConstitution,
  govActionGenerators,
  genBabelPlutusPurposePointer,
  genGovAction,
  genGovActionState,
  genPParamUpdateGovAction,
  genHardForkGovAction,
  genCommitteeGovAction,
  genConstitutionGovAction,
  genProposals,
  ProposalsNewActions (..),
  ProposalsForEnactment (..),
  ShuffledGovActionStates (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..))
import Cardano.Ledger.Babel.Rules
import Cardano.Ledger.Babel.Scripts (BabelPlutusPurpose (..))
import Cardano.Ledger.Babel.TxBody (BabelTxBody (BabelTxBody))
import Cardano.Ledger.Babel.TxCert
import Cardano.Ledger.Babel.TxInfo (BabelContextError)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (Sized)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Crypto (Crypto)
import Control.State.Transition.Extended (STS (PredicateFailure))
import Data.Default.Class (def)
import Data.Foldable (toList)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Word
import Generic.Random (genericArbitraryU)
import Lens.Micro
import Test.Cardano.Data.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

instance
  ( BabelEraTxBody era
  , Arbitrary (Sized (TxOut era))
  , Arbitrary (TxOut era)
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , Arbitrary (PParamsUpdate era)
  ) =>
  Arbitrary (BabelTxBody era)
  where
  arbitrary =
    BabelTxBody
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
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( EraPParams era
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (PlutusPurpose AsIx era)
  , Arbitrary (TxCert era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (BabelContextError era)
  where
  arbitrary = genericArbitraryU

instance Crypto c => Arbitrary (BabelGenesis c) where
  arbitrary =
    BabelGenesis
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Crypto c => Arbitrary (BabelDelegCert c) where
  arbitrary =
    oneof
      [ BabelRegCert <$> arbitrary <*> arbitrary
      , BabelUnRegCert <$> arbitrary <*> arbitrary
      , BabelDelegCert <$> arbitrary <*> arbitrary
      , BabelRegDelegCert <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Crypto c => Arbitrary (BabelGovCert c) where
  arbitrary =
    oneof
      [ BabelRegDRep <$> arbitrary <*> arbitrary <*> arbitrary
      , BabelUnRegDRep <$> arbitrary <*> arbitrary
      , BabelAuthCommitteeHotKey <$> arbitrary <*> arbitrary
      , BabelResignCommitteeColdKey <$> arbitrary <*> arbitrary
      ]

instance
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (BabelUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
  , Arbitrary (TxCert era)
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (BabelUtxowPredFailure era)
  where
  arbitrary = genericArbitraryU

_uniqueIdGovActions ::
  (Era era, Arbitrary (PParamsUpdate era)) =>
  Gen (SSeq.StrictSeq (GovActionState era))
_uniqueIdGovActions = SSeq.fromList . nubBy (\x y -> gasId x == gasId y) <$> arbitrary

data ProposalsForEnactment era
  = ProposalsForEnactment
      (Proposals era)
      (Seq.Seq (GovActionState era))
      (Set.Set (GovActionId (EraCrypto era)))
  deriving (Show, Eq)

instance
  (EraPParams era, Arbitrary (PParamsUpdate era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (ProposalsForEnactment era)
  where
  arbitrary = do
    ps <- genProposals @era (2, 50)
    pparamUpdates <- chooseLineage grPParamUpdateL ps Seq.Empty
    hardForks <- chooseLineage grHardForkL ps Seq.Empty
    committees <- chooseLineage grCommitteeL ps Seq.Empty
    constitutions <- chooseLineage grConstitutionL ps Seq.Empty
    sequencedGass <-
      sequenceLineages
        ( Seq.filter
            (not . Seq.null)
            (Seq.fromList [pparamUpdates, hardForks, committees, constitutions])
        )
        Seq.Empty
    let expiredGais =
          Set.fromList (toList $ proposalsIds ps)
            `Set.difference` Set.fromList (gasId <$> toList sequencedGass)
    pure $ ProposalsForEnactment ps sequencedGass expiredGais
    where
      chooseLineage ::
        (forall f. Lens' (GovRelation f era) (f (GovPurposeId p era))) ->
        Proposals era ->
        Seq.Seq (GovActionState era) ->
        Gen (Seq.Seq (GovActionState era))
      chooseLineage govRelL ps = \case
        Seq.Empty ->
          let children = ps ^. pRootsL . govRelL . prChildrenL
           in if Set.null children
                then pure Seq.Empty
                else do
                  child <- elements $ toList children
                  chooseLineage govRelL ps (Seq.Empty Seq.:|> (proposalsActionsMap ps Map.! unGovPurposeId child))
        lineage@(_ Seq.:|> gas) ->
          let children = ps ^. pGraphL . govRelL . pGraphNodesL . to (Map.! GovPurposeId (gasId gas)) . peChildrenL
           in if Set.null children
                then pure lineage
                else do
                  child <- elements $ toList children
                  chooseLineage govRelL ps (lineage Seq.:|> (proposalsActionsMap ps Map.! unGovPurposeId child))
      consumeHeadAtIndex :: Int -> Seq.Seq (Seq.Seq a) -> (a, Seq.Seq (Seq.Seq a))
      consumeHeadAtIndex idx ss = (ss `Seq.index` idx `Seq.index` 0, Seq.adjust' (Seq.drop 1) idx ss)
      sequenceLineages :: Seq.Seq (Seq.Seq a) -> Seq.Seq a -> Gen (Seq.Seq a)
      sequenceLineages lineages sequenced = case lineages of
        Seq.Empty -> pure sequenced
        _ -> do
          index <- chooseInt (0, length lineages - 1)
          let (chosen, adjustedLineages) = consumeHeadAtIndex index lineages
          sequenceLineages (Seq.filter (not . Seq.null) adjustedLineages) (sequenced Seq.:|> chosen)

data ProposalsNewActions era = ProposalsNewActions (Proposals era) [GovActionState era]
  deriving (Show, Eq)

instance
  (EraPParams era, Arbitrary (PParamsUpdate era), Arbitrary (PParamsHKD StrictMaybe era)) =>
  Arbitrary (ProposalsNewActions era)
  where
  arbitrary = do
    ps <- arbitrary
    i <- chooseInt (2, 20)
    gass <- vectorOf i $ genGovActionState =<< genGovAction ps
    pure $ ProposalsNewActions ps gass

genProposals ::
  forall era.
  ( HasCallStack
  , EraPParams era
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
    [ genWithParent genPParamUpdateGovAction grPParamUpdateL
    , genWithParent genHardForkGovAction grHardForkL
    , genTreasuryWithdrawals
    , genWithParent genCommitteeGovAction grCommitteeL
    , genWithParent genConstitutionGovAction grConstitutionL
    , pure InfoAction
    ]
  where
    genWithParent ::
      (StrictMaybe (GovPurposeId p era) -> Gen (GovAction era)) ->
      (forall f. Lens' (GovRelation f era) (f (GovPurposeId p era))) ->
      Gen (GovAction era)
    genWithParent gen govRelL =
      gen
        =<< elements
          ( (ps ^. pRootsL . govRelL . prRootL)
              : fmap SJust (Map.keys $ ps ^. pGraphL . govRelL . pGraphNodesL)
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
    <*> (ProposalProcedure <$> arbitrary <*> arbitrary <*> pure ga <*> arbitrary)
    <*> arbitrary
    <*> arbitrary

-- | These lists of `GovActionStates` contain only one of a priority.
-- In other words, no two `GovActionState`s in the list have the same `actionPriority`.
data ShuffledGovActionStates era
  = ShuffledGovActionStates [GovActionState era] [GovActionState era]
  deriving (Show)

instance
  (Era era, Arbitrary (PParamsUpdate era)) =>
  Arbitrary (ShuffledGovActionStates era)
  where
  arbitrary = do
    gass <- traverse (genGovActionState =<<) govActionGenerators
    shuffledGass <- shuffle gass
    pure $ ShuffledGovActionStates gass shuffledGass

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

instance
  ( Era era
  , Arbitrary (TxCert era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (BabelPlutusPurpose AsItem era)
  where
  arbitrary =
    oneof
      [ BabelSpending <$> arbitrary
      , BabelMinting <$> arbitrary
      , BabelCertifying <$> arbitrary
      , BabelRewarding <$> arbitrary
      , BabelVoting <$> arbitrary
      , BabelProposing <$> arbitrary
      ]

instance
  ( Era era
  , Arbitrary (TxCert era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  ) =>
  Arbitrary (BabelPlutusPurpose AsIxItem era)
  where
  arbitrary =
    oneof
      [ BabelSpending <$> arbitrary
      , BabelMinting <$> arbitrary
      , BabelCertifying <$> arbitrary
      , BabelRewarding <$> arbitrary
      , BabelVoting <$> arbitrary
      , BabelProposing <$> arbitrary
      ]

instance
  Era era =>
  Arbitrary (BabelPlutusPurpose AsIx era)
  where
  arbitrary = arbitrary >>= genBabelPlutusPurposePointer

genBabelPlutusPurposePointer :: Word32 -> Gen (BabelPlutusPurpose AsIx era)
genBabelPlutusPurposePointer i =
  elements
    [ BabelSpending (AsIx i)
    , BabelMinting (AsIx i)
    , BabelCertifying (AsIx i)
    , BabelRewarding (AsIx i)
    , BabelVoting (AsIx i)
    , BabelProposing (AsIx i)
    ]

------------------------------------------------------------------------------------------
-- Cardano.Ledger.Babel.Rules -----------------------------------------------------------
------------------------------------------------------------------------------------------

instance
  ( Era era
  , Arbitrary (CollectError era)
  ) =>
  Arbitrary (BabelUtxosPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXOW" era))
  , Arbitrary (PredicateFailure (EraRule "CERTS" era))
  , Arbitrary (PredicateFailure (EraRule "GOV" era))
  ) =>
  Arbitrary (BabelLedgerPredFailure era)
  where
  arbitrary = genericArbitraryU
