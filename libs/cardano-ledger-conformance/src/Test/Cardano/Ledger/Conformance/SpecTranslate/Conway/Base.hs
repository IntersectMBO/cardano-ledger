{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  committeeCredentialToStrictMaybe,
  SpecTranslate (..),
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (
  Timelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (AlonzoTxAuxData, MaryValue)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..), unTxDats)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..), ConwayPParams (..), THKD (..))
import Cardano.Ledger.Conway.Rules (EnactSignal (..))
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..), ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Shelley.Rules (Identity)
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad.Except (MonadError (..))
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict (OMap)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import qualified GHC.Exts as Exts
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.Orphans ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTransM,
  SpecTranslate (..),
  askCtx,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (committeeCredentialToStrictMaybe)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..), showExpr)

instance
  ( AlonzoEraScript era
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate ctx (Timelock era)
  where
  type SpecRep (Timelock era) = Agda.HSTimelock

  toSpecRep tl =
    Agda.HSTimelock
      <$> timelockToSpecRep tl
      <*> toSpecRep (hashScript @era $ NativeScript tl)
      <*> pure (fromIntegral $ originalBytesSize tl)
    where
      timelockToSpecRep x =
        case x of
          RequireSignature kh ->
            Agda.RequireSig <$> toSpecRep kh
          RequireAllOf ss -> do
            tls <- traverse timelockToSpecRep ss
            pure . Agda.RequireAllOf $ toList tls
          RequireAnyOf ss -> do
            tls <- traverse timelockToSpecRep ss
            pure . Agda.RequireAnyOf $ toList tls
          RequireMOf m ss -> do
            tls <- traverse timelockToSpecRep ss
            pure . Agda.RequireMOf (toInteger m) $ toList tls
          RequireTimeExpire slot -> Agda.RequireTimeExpire <$> toSpecRep slot
          RequireTimeStart slot -> Agda.RequireTimeStart <$> toSpecRep slot
          _ -> error "Impossible: All NativeScripts should have been accounted for"

instance
  ( AlonzoEraScript era
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate ctx (PlutusScript era)
  where
  type SpecRep (PlutusScript era) = Agda.HSPlutusScript

  toSpecRep ps =
    Agda.MkHSPlutusScript
      <$> toSpecRep (hashScript $ PlutusScript ps)
      <*> pure (fromIntegral $ originalBytesSize ps)

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate ctx (AlonzoScript era)
  where
  type SpecRep (AlonzoScript era) = Agda.Script

  toSpecRep (NativeScript s) = Left <$> toSpecRep s
  toSpecRep (PlutusScript s) = Right <$> toSpecRep s

instance
  ( EraTxOut era
  , SpecRep (Value era) ~ Agda.Coin
  , Script era ~ AlonzoScript era
  , SpecTranslate ctx (Value era)
  , SpecTranslate ctx (Script era)
  ) =>
  SpecTranslate ctx (BabbageTxOut era)
  where
  type SpecRep (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum script) = do
    addr' <- toSpecRep addr
    val' <- toSpecRep val
    datum' <- toSpecRep datum
    script' <- toSpecRep script
    pure (addr', (val', (datum', script')))

instance
  ( SpecTranslate ctx (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate ctx (UTxO era)
  where
  type SpecRep (UTxO era) = SpecRep (Map TxIn (TxOut era))

  toSpecRep (UTxO m) = toSpecRep m

deriving instance SpecTranslate ctx OrdExUnits

deriving instance SpecTranslate ctx CoinPerByte

instance
  SpecTranslate ctx (HKD f a) =>
  SpecTranslate ctx (THKD r f a)
  where
  type SpecRep (THKD r f a) = SpecRep (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate ctx DRepVotingThresholds where
  type SpecRep DRepVotingThresholds = Agda.DrepThresholds

  toSpecRep DRepVotingThresholds {..} =
    Agda.MkDrepThresholds
      <$> toSpecRep dvtMotionNoConfidence
      <*> toSpecRep dvtCommitteeNormal
      <*> toSpecRep dvtCommitteeNoConfidence
      <*> toSpecRep dvtUpdateToConstitution
      <*> toSpecRep dvtHardForkInitiation
      <*> toSpecRep dvtPPNetworkGroup
      <*> toSpecRep dvtPPEconomicGroup
      <*> toSpecRep dvtPPTechnicalGroup
      <*> toSpecRep dvtPPGovGroup
      <*> toSpecRep dvtTreasuryWithdrawal

instance SpecTranslate ctx PoolVotingThresholds where
  type SpecRep PoolVotingThresholds = Agda.PoolThresholds

  toSpecRep PoolVotingThresholds {..} =
    Agda.MkPoolThresholds
      <$> toSpecRep pvtMotionNoConfidence
      <*> toSpecRep pvtCommitteeNormal
      <*> toSpecRep pvtCommitteeNoConfidence
      <*> toSpecRep pvtHardForkInitiation
      <*> toSpecRep pvtPPSecurityGroup

instance
  ( ConwayEraPParams era
  , PParamsHKD Identity era ~ ConwayPParams Identity era
  ) =>
  SpecTranslate ctx (ConwayPParams Identity era)
  where
  type SpecRep (ConwayPParams Identity era) = Agda.PParams

  toSpecRep cpp@ConwayPParams {..} = do
    ppA <- toSpecRep cppMinFeeA
    ppB <- toSpecRep cppMinFeeB
    ppA0 <- toSpecRep cppA0
    ppMinFeeRefScriptCoinsPerByte <- toSpecRep cppMinFeeRefScriptCostPerByte
    ppCollateralPercentage <- toSpecRep cppCollateralPercentage
    let
      ppMaxBlockSize = toInteger $ unTHKD cppMaxBBSize
      ppMaxTxSize = toInteger $ unTHKD cppMaxTxSize
      ppMaxHeaderSize = toInteger $ unTHKD cppMaxBHSize
    ppKeyDeposit <- toSpecRep cppKeyDeposit
    ppPoolDeposit <- toSpecRep cppPoolDeposit
    ppEmax <- toSpecRep cppEMax
    ppNopt <- toSpecRep (toInteger $ unTHKD cppNOpt)
    let
      -- We don't really care about `ppPv` in conformance testing, because
      -- the actual protocol version is stored elsewhere starting from Conway
      -- and this is just here for backwards compatibility
      ppPv = (0, 0)
      ppMinUTxOValue = 0 -- minUTxOValue has been deprecated and is not supported in Conway
    ppCoinsPerUTxOByte <- toSpecRep cppCoinsPerUTxOByte
    ppCostmdls <- toSpecRep cppCostModels
    ppPrices <- toSpecRep cppPrices
    let
      pp = PParams cpp
      ppMaxRefScriptSizePerTx = toInteger $ pp ^. ppMaxRefScriptSizePerTxG
      ppMaxRefScriptSizePerBlock = toInteger $ pp ^. ppMaxRefScriptSizePerBlockG
      ppRefScriptCostStride = toInteger . unNonZero $ pp ^. ppRefScriptCostStrideG
      ppRefScriptCostMultiplier = unboundRational $ pp ^. ppRefScriptCostMultiplierG
    ppMaxTxExUnits <- toSpecRep cppMaxTxExUnits
    ppMaxBlockExUnits <- toSpecRep cppMaxBlockExUnits
    let
      ppMaxValSize = toInteger . unTHKD $ cppMaxValSize
      ppMaxCollateralInputs = toInteger . unTHKD $ cppMaxCollateralInputs
    ppPoolThresholds <- toSpecRep cppPoolVotingThresholds
    ppDrepThresholds <- toSpecRep cppDRepVotingThresholds
    let
      ppCcMinSize = toInteger . unTHKD $ cppCommitteeMinSize
      ppCcMaxTermLength = toInteger . unEpochInterval . unTHKD $ cppCommitteeMaxTermLength
    ppGovActionLifetime <- toSpecRep cppGovActionLifetime
    ppGovActionDeposit <- toSpecRep cppGovActionDeposit
    ppDrepDeposit <- toSpecRep cppDRepDeposit
    ppDrepActivity <- toSpecRep cppDRepActivity
    ppMonetaryExpansion <- toSpecRep cppRho
    ppTreasuryCut <- toSpecRep cppTau

    pure Agda.MkPParams {..}

instance SpecTranslate ctx ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance Era era => SpecTranslate ctx (TxDats era) where
  type SpecRep (TxDats era) = Agda.HSSet Agda.Datum

  toSpecRep = fmap Agda.MkHSSet . traverse (toSpecRep . snd) . Map.toList . unTxDats

instance SpecTranslate ctx (AlonzoPlutusPurpose AsIx era) where
  type SpecRep (AlonzoPlutusPurpose AsIx era) = Agda.RdmrPtr

  toSpecRep = \case
    AlonzoSpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    AlonzoMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    AlonzoCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    AlonzoRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)

instance SpecTranslate ctx (ConwayPlutusPurpose AsIx era) where
  type SpecRep (ConwayPlutusPurpose AsIx era) = Agda.RdmrPtr

  toSpecRep = \case
    ConwaySpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    ConwayMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    ConwayCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    ConwayRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)
    ConwayVoting (AsIx i) -> pure (Agda.Vote, toInteger i)
    ConwayProposing (AsIx i) -> pure (Agda.Propose, toInteger i)

instance
  ( AlonzoEraScript era
  , SpecTranslate ctx (PlutusPurpose AsIx era)
  ) =>
  SpecTranslate ctx (Redeemers era)
  where
  type
    SpecRep (Redeemers era) =
      Agda.HSMap (SpecRep (PlutusPurpose AsIx era)) (Agda.Redeemer, Agda.ExUnits)

  toSpecRep (Redeemers x) = toSpecRep x

instance
  ( AlonzoEraScript era
  , SpecTranslate ctx (PlutusPurpose AsIx era)
  , SpecRep (PlutusPurpose AsIx era) ~ Agda.RdmrPtr
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate ctx (AlonzoTxWits era)
  where
  type SpecRep (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> fmap Agda.MkHSMap (toSpecRep txWitsMap)
      <*> fmap Agda.MkHSSet (toSpecRep (Map.elems $ txscripts x))
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)
    where
      txWitsMap = toList (txwitsVKey x)

instance Era era => SpecTranslate ctx (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate ctx StakePoolParams where
  type SpecRep StakePoolParams = Agda.StakePoolParams

  toSpecRep StakePoolParams {..} =
    Agda.StakePoolParams
      <$> toSpecRep sppOwners
      <*> toSpecRep sppCost
      <*> toSpecRep sppMargin
      <*> toSpecRep sppPledge
      <*> toSpecRep (raCredential sppRewardAccount)

instance SpecTranslate ctx DRep where
  type SpecRep DRep = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.VDelegCredential <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.VDelegAbstain
  toSpecRep DRepAlwaysNoConfidence = pure Agda.VDelegNoConfidence

instance SpecTranslate ctx Url where
  type SpecRep Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate ctx Anchor where
  type SpecRep Anchor = Agda.Anchor
  toSpecRep (Anchor url h) = Agda.Anchor <$> toSpecRep url <*> toSpecRep h

instance SpecTranslate ctx Withdrawals where
  type SpecRep Withdrawals = Agda.Withdrawals

  toSpecRep (Withdrawals w) = toSpecRep w

instance SpecTranslate ctx IsValid where
  type SpecRep IsValid = Bool

  toSpecRep (IsValid b) = pure b

instance SpecTranslate ctx (GovPurposeId r) where
  type SpecRep (GovPurposeId r) = (Agda.TxId, Integer)

  toSpecRep (GovPurposeId gaId) = toSpecRep gaId

instance SpecTranslate ctx (Committee era) where
  type SpecRep (Committee era) = (Agda.HSMap Agda.Credential Agda.Epoch, Agda.Rational)

  toSpecRep (Committee members threshold) = toSpecRep (members, threshold)

instance SpecTranslate ctx (Constitution era) where
  type SpecRep (Constitution era) = (Agda.DataHash, Maybe Agda.ScriptHash)

  toSpecRep (Constitution (Anchor _ h) policy) = toSpecRep (h, policy)

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx (EnactState era)
  where
  type SpecRep (EnactState era) = Agda.EnactState

  toSpecRep EnactState {..} =
    Agda.MkEnactState
      <$> transHashProtected ensCommittee grCommittee
      <*> transHashProtected ensConstitution grConstitution
      <*> transHashProtected (ensCurPParams ^. ppProtocolVersionL) grHardFork
      <*> transHashProtected ensCurPParams grPParamUpdate
      <*> transWithdrawals ensWithdrawals
    where
      GovRelation {..} = ensPrevGovActionIds
      transWithdrawals ws = fmap Agda.MkHSMap . forM (Map.toList ws) $
        \(cred, Coin amount) -> do
          agdaCred <- toSpecRep cred
          network <- toSpecRep Testnet -- TODO where should this really come from?
          pure (Agda.RwdAddr network agdaCred, amount)
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance SpecTranslate ctx Voter where
  type SpecRep Voter = Agda.GovVoter

  toSpecRep (CommitteeVoter c) = (Agda.CC,) <$> toSpecRep c
  toSpecRep (DRepVoter c) = (Agda.DRep,) <$> toSpecRep c
  toSpecRep (StakePoolVoter kh) = (Agda.SPO,) <$> toSpecRep (KeyHashObj kh)

instance SpecTranslate ctx Vote where
  type SpecRep Vote = Agda.Vote

  toSpecRep VoteYes = pure Agda.Yes
  toSpecRep VoteNo = pure Agda.No
  toSpecRep Abstain = pure Agda.Abstain

instance SpecTranslate ctx (VotingProcedures era) where
  type SpecRep (VotingProcedures era) = [Agda.GovVote]

  toSpecRep = foldrVotingProcedures go (pure [])
    where
      go ::
        Voter ->
        GovActionId ->
        VotingProcedure era ->
        SpecTransM ctx [Agda.GovVote] ->
        SpecTransM ctx [Agda.GovVote]
      go voter gaId votingProcedure m =
        (:)
          <$> ( Agda.MkGovVote
                  <$> toSpecRep gaId
                  <*> toSpecRep voter
                  <*> toSpecRep (vProcVote votingProcedure)
                  <*> toSpecRep (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate ctx (ConwayPParams StrictMaybe era) where
  type SpecRep (ConwayPParams StrictMaybe era) = Agda.PParamsUpdate

  toSpecRep (ConwayPParams {..}) = do
    ppuA <- toSpecRep cppMinFeeA
    ppuB <- toSpecRep cppMinFeeB
    ppuA0 <- toSpecRep cppA0
    ppuMinFeeRefScriptCoinsPerByte <- toSpecRep cppMinFeeRefScriptCostPerByte
    ppuCollateralPercentage <- toSpecRep cppCollateralPercentage
    let
      ppuMaxBlockSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxBBSize
      ppuMaxTxSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxTxSize
      ppuMaxHeaderSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxBHSize
    ppuKeyDeposit <- toSpecRep cppKeyDeposit
    ppuPoolDeposit <- toSpecRep cppPoolDeposit
    ppuEmax <- toSpecRep cppEMax
    ppuNopt <- toSpecRep (fmap toInteger . strictMaybeToMaybe $ unTHKD cppNOpt)
    let
      ppuPv = Nothing
      ppuMinUTxOValue = Nothing -- minUTxOValue has been deprecated and is not supported in Conway
    ppuCoinsPerUTxOByte <- toSpecRep cppCoinsPerUTxOByte
    ppuCostmdls <- toSpecRep cppCostModels
    ppuPrices <- toSpecRep cppPrices
    let
      ppuMaxRefScriptSizePerTx = Nothing
      ppuMaxRefScriptSizePerBlock = Nothing
      ppuRefScriptCostStride = Nothing
      ppuRefScriptCostMultiplier = Nothing
    ppuMaxTxExUnits <- toSpecRep cppMaxTxExUnits
    ppuMaxBlockExUnits <- toSpecRep cppMaxBlockExUnits
    let
      ppuMaxValSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxValSize
      ppuMaxCollateralInputs = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxCollateralInputs
    ppuPoolThresholds <- toSpecRep cppPoolVotingThresholds
    ppuDrepThresholds <- toSpecRep cppDRepVotingThresholds
    let
      ppuCcMinSize = fmap toInteger . strictMaybeToMaybe $ unTHKD cppCommitteeMinSize
      ppuCcMaxTermLength =
        fmap (toInteger . unEpochInterval) . strictMaybeToMaybe $ unTHKD cppCommitteeMaxTermLength
    ppuGovActionLifetime <- toSpecRep cppGovActionLifetime
    ppuGovActionDeposit <- toSpecRep cppGovActionDeposit
    ppuDrepDeposit <- toSpecRep cppDRepDeposit
    ppuDrepActivity <- toSpecRep cppDRepActivity
    ppuMonetaryExpansion <- toSpecRep cppRho
    ppuTreasuryCut <- toSpecRep cppTau

    pure Agda.MkPParamsUpdate {..}

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (GovAction era)
  where
  type SpecRep (GovAction era) = Agda.GovAction

  toSpecRep (ParameterChange _ ppu _) = Agda.ChangePParams <$> toSpecRep ppu
  toSpecRep (HardForkInitiation _ pv) = Agda.TriggerHardFork <$> toSpecRep pv
  toSpecRep (TreasuryWithdrawals withdrawals _) =
    Agda.TreasuryWithdrawal
      <$> toSpecRep withdrawals
  toSpecRep (NoConfidence _) = pure Agda.NoConfidence
  toSpecRep (UpdateCommittee _ remove add threshold) =
    Agda.UpdateCommittee
      <$> toSpecRep add
      <*> toSpecRep remove
      <*> toSpecRep threshold
  toSpecRep (NewConstitution _ (Constitution (Anchor _ h) policy)) =
    Agda.NewConstitution
      <$> toSpecRep h
      <*> toSpecRep policy
  toSpecRep InfoAction = pure Agda.Info

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (ProposalProcedure era)
  where
  type SpecRep (ProposalProcedure era) = Agda.GovProposal

  toSpecRep ProposalProcedure {..} =
    Agda.MkGovProposal
      <$> toSpecRep pProcGovAction
      <*> toSpecRep (nullifyIfNotNeeded prevActionId pProcGovAction)
      <*> toSpecRep policy
      <*> toSpecRep pProcDeposit
      <*> toSpecRep pProcReturnAddr
      <*> toSpecRep pProcAnchor
    where
      prevActionId = prevGovActionId pProcGovAction
      policy =
        case pProcGovAction of
          TreasuryWithdrawals _ sh -> sh
          ParameterChange _ _ sh -> sh
          _ -> SNothing

prevGovActionId :: GovAction era -> StrictMaybe GovActionId
prevGovActionId action =
  case action of
    ParameterChange x _ _ -> unGovPurposeId <$> x
    HardForkInitiation x _ -> unGovPurposeId <$> x
    UpdateCommittee x _ _ _ -> unGovPurposeId <$> x
    NoConfidence x -> unGovPurposeId <$> x
    NewConstitution x _ -> unGovPurposeId <$> x
    _ -> SNothing

nullGovActionId :: GovActionId
nullGovActionId = GovActionId (TxId def) (GovActionIx 0)

nullifyIfNotNeeded :: StrictMaybe GovActionId -> GovAction era -> GovActionId
nullifyIfNotNeeded SNothing = const nullGovActionId
nullifyIfNotNeeded (SJust gaId) = \case
  TreasuryWithdrawals {} -> nullGovActionId
  InfoAction -> nullGovActionId
  _ -> gaId

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (GovActionState era)
  where
  type SpecRep (GovActionState era) = Agda.GovActionState

  toSpecRep gas@GovActionState {..} = do
    Agda.MkGovActionState
      <$> ( Agda.GovVotes
              <$> toSpecRep gasCommitteeVotes
              <*> toSpecRep gasDRepVotes
              <*> toSpecRep gasStakePoolVotes
          )
      <*> toSpecRep (gasReturnAddr gas)
      <*> toSpecRep gasExpiresAfter
      <*> toSpecRep action
      <*> toSpecRep (nullifyIfNotNeeded (prevGovActionId action) action)
    where
      action = gasAction gas

instance SpecTranslate ctx GovActionIx where
  type SpecRep GovActionIx = Integer

  toSpecRep = pure . fromIntegral . unGovActionIx

instance SpecTranslate ctx GovActionId where
  type SpecRep GovActionId = Agda.GovActionID

  toSpecRep (GovActionId txId gaIx) = toSpecRep (txId, gaIx)

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (Proposals era)
  where
  type SpecRep (Proposals era) = Agda.GovState

  -- TODO get rid of `prioritySort` once we've changed the implementation so
  -- that the proposals are always sorted
  toSpecRep = toSpecRep . prioritySort . view pPropsL
    where
      prioritySort ::
        OMap GovActionId (GovActionState era) ->
        OMap GovActionId (GovActionState era)
      prioritySort = Exts.fromList . sortOn (actionPriority . gasAction) . Exts.toList

instance SpecTranslate ctx MaryValue where
  type SpecRep MaryValue = Agda.Coin

  toSpecRep = toSpecRep . coin

instance
  (Inject ctx Coin, ConwayEraAccounts era) =>
  SpecTranslate ctx (RatifyEnv era)
  where
  type SpecRep (RatifyEnv era) = Agda.RatifyEnv

  toSpecRep RatifyEnv {..} = do
    let
      stakeDistrs =
        Agda.StakeDistrs
          <$> toSpecRep reDRepDistr
          <*> toSpecRep reStakePoolDistr
      dreps = toSpecRep $ Map.map drepExpiry reDRepState
    treasury <- askCtx @Coin
    Agda.MkRatifyEnv
      <$> stakeDistrs
      <*> toSpecRep reCurrentEpoch
      <*> dreps
      <*> toSpecRep reCommitteeState
      <*> toSpecRep treasury
      <*> toSpecRep (Map.mapWithKey stakePoolStateToStakePoolParams reStakePools)
      <*> toSpecRep (Map.mapMaybe (^. dRepDelegationAccountStateL) (reAccounts ^. accountsMapL))

instance
  ( EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx [GovActionState era]
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  ) =>
  SpecTranslate ctx (RatifyState era)
  where
  type SpecRep (RatifyState era) = Agda.RatifyState

  toSpecRep RatifyState {..} = do
    govActionMap <-
      foldl' (\acc gas -> Map.insert (gasId gas) gas acc) mempty
        <$> askCtx @[GovActionState era]
    let
      lookupGAS gaId m = do
        case Map.lookup gaId govActionMap of
          Just x -> Set.insert (gaId, x) <$> m
          Nothing ->
            throwError $
              "gaId: "
                <> T.pack (showExpr gaId)
                <> "\n\ngovActionMap: "
                <> T.pack (showExpr govActionMap)
                <> "\n\nGovActionId is not contained in the govActionMap"
    removed <-
      Set.foldr'
        lookupGAS
        (pure Set.empty)
        (rsExpired `Set.union` Set.fromList (gasId <$> toList rsEnacted))
    Agda.MkRatifyState
      <$> toSpecRep rsEnactState
      <*> toSpecRep removed
      <*> toSpecRep rsDelayed

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (RatifySignal era)
  where
  type
    SpecRep (RatifySignal era) =
      SpecRep [(GovActionId, GovActionState era)]

  toSpecRep (RatifySignal x) =
    toSpecRep $
      (\gas@GovActionState {gasId} -> (gasId, gas)) <$> x

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (EnactSignal era)
  where
  type SpecRep (EnactSignal era) = SpecRep (GovAction era)

  toSpecRep (EnactSignal _ ga) = toSpecRep ga
