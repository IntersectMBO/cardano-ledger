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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra.Base (
  committeeCredentialToStrictMaybe,
  SpecTranslate (..),
) where

import Cardano.Ledger.Address (
  DirectDeposits (..),
  accountAddressCredentialL,
 )
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (AlonzoTxAuxData, MaryValue)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (plutusScriptLanguage)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..), unTxDats)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..), THKD (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams (..))
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval (..),
  AccountBalanceIntervals (..),
  DijkstraNativeScript,
  DijkstraPlutusPurpose (..),
  pattern RequireGuard,
 )
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Plutus.CostModels (CostModels, costModelsValid)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum (..), hashBinaryData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
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
import Data.Functor.Identity (Identity)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict (OMap)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import qualified GHC.Exts as Exts
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.Orphans.Core ()
import Test.Cardano.Ledger.Conformance.Orphans.Dijkstra ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (committeeCredentialToStrictMaybe)
import Test.Cardano.Ledger.Conway.TreeDiff (showExpr)
import Test.Cardano.Ledger.Dijkstra.TreeDiff ()

instance SpecTranslate DijkstraEra TxId where
  type SpecRep DijkstraEra TxId = Agda.TxId

  toSpecRep (TxId x) = toSpecRep x

instance SpecTranslate DijkstraEra TxIn where
  type SpecRep DijkstraEra TxIn = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRepTuple (txId, txIx)

instance SpecTranslate DijkstraEra (SafeHash a) where
  type SpecRep DijkstraEra (SafeHash a) = Agda.DataHash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate DijkstraEra Language where
  type SpecRep DijkstraEra Language = Agda.HSLanguage

  toSpecRep l = case l of
    PlutusV1 -> return Agda.PV1
    PlutusV2 -> return Agda.PV2
    PlutusV3 -> return Agda.PV3
    PlutusV4 -> error "PlutusV4 not supported"

instance SpecTranslate DijkstraEra CostModels where
  type SpecRep DijkstraEra CostModels = Agda.LanguageCostModels

  toSpecRep cm =
    Agda.MkLanguageCostModels
      <$> mapM (\(l, _) -> (,()) <$> toSpecRep l) (Map.toList (costModelsValid cm))

instance SpecTranslate DijkstraEra ExUnits where
  type SpecRep DijkstraEra ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

instance SpecTranslate DijkstraEra (BinaryData DijkstraEra) where
  type SpecRep DijkstraEra (BinaryData DijkstraEra) = Agda.DataHash

  toSpecRep = toSpecRep . hashBinaryData

instance SpecTranslate DijkstraEra (Datum DijkstraEra) where
  type SpecRep DijkstraEra (Datum DijkstraEra) = Maybe (Either Agda.Datum Agda.DataHash)

  toSpecRep NoDatum = pure Nothing
  toSpecRep (Datum d) = Just . Left <$> toSpecRep d
  toSpecRep (DatumHash h) = Just . Right <$> toSpecRep h

instance SpecTranslate DijkstraEra (Data DijkstraEra) where
  type SpecRep DijkstraEra (Data DijkstraEra) = Agda.DataHash

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate DijkstraEra TxAuxDataHash where
  type SpecRep DijkstraEra TxAuxDataHash = Agda.DataHash

  toSpecRep (TxAuxDataHash x) = toSpecRep x

instance SpecTranslate DijkstraEra (DijkstraNativeScript DijkstraEra) where
  type SpecRep DijkstraEra (DijkstraNativeScript DijkstraEra) = Agda.HSNativeScript

  toSpecRep ns =
    Agda.HSNativeScript
      <$> nativeScriptToSpecRep ns
      <*> toSpecRep (hashScript $ NativeScript ns)
      <*> pure (fromIntegral $ originalBytesSize ns)
    where
      nativeScriptToSpecRep x =
        case x of
          RequireSignature kh ->
            Agda.RequireSig <$> toSpecRep kh
          RequireAllOf ss -> do
            tls <- traverse nativeScriptToSpecRep ss
            pure . Agda.RequireAllOf $ toList tls
          RequireAnyOf ss -> do
            tls <- traverse nativeScriptToSpecRep ss
            pure . Agda.RequireAnyOf $ toList tls
          RequireMOf m ss -> do
            tls <- traverse nativeScriptToSpecRep ss
            pure . Agda.RequireMOf (toInteger m) $ toList tls
          RequireTimeExpire slot -> Agda.RequireTimeExpire <$> toSpecRep slot
          RequireTimeStart slot -> Agda.RequireTimeStart <$> toSpecRep slot
          RequireGuard cred -> Agda.RequireGuard <$> toSpecRep cred
          _ -> error "Impossible: All NativeScripts should have been accounted for"

instance SpecTranslate DijkstraEra (PlutusScript DijkstraEra) where
  type SpecRep DijkstraEra (PlutusScript DijkstraEra) = Agda.HSPlutusScript

  toSpecRep ps =
    Agda.MkHSPlutusScript
      <$> toSpecRep (hashScript $ PlutusScript ps)
      <*> pure (fromIntegral $ originalBytesSize ps)
      <*> toSpecRep (plutusScriptLanguage ps)

instance SpecTranslate DijkstraEra DirectDeposits where
  type SpecRep DijkstraEra DirectDeposits = Agda.HSMap Agda.RewardAddress Integer

  toSpecRep (DirectDeposits m) = toSpecRepMap m

instance SpecTranslate DijkstraEra (AccountBalanceInterval DijkstraEra) where
  type SpecRep DijkstraEra (AccountBalanceInterval DijkstraEra) = Agda.BalanceInterval

  toSpecRep = \case
    AccountBalanceLowerBound (Inclusive (Coin l)) ->
      pure $ Agda.Lower l
    AccountBalanceUpperBound (Exclusive (Coin u)) ->
      pure $ Agda.Upper u
    AccountBalanceBothBounds (Inclusive (Coin l)) (Exclusive (Coin u)) ->
      pure $ Agda.Both l u

instance SpecTranslate DijkstraEra (AccountBalanceIntervals DijkstraEra) where
  type
    SpecRep DijkstraEra (AccountBalanceIntervals DijkstraEra) =
      Agda.HSMap Agda.Credential Agda.BalanceInterval

  toSpecRep (AccountBalanceIntervals m) =
    toSpecRepMap $ Map.mapKeys (\(AccountId c) -> c) m

instance SpecTranslate DijkstraEra (AlonzoScript DijkstraEra) where
  type SpecRep DijkstraEra (AlonzoScript DijkstraEra) = Agda.Script

  toSpecRep (NativeScript s) = Left <$> toSpecRep s
  toSpecRep (PlutusScript s) = Right <$> toSpecRep s

instance SpecTranslate DijkstraEra (BabbageTxOut DijkstraEra) where
  type SpecRep DijkstraEra (BabbageTxOut DijkstraEra) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum script) = do
    addr' <- toSpecRep addr
    val' <- toSpecRep val
    datum' <- toSpecRep datum
    script' <- toSpecRep script
    pure (addr', (val', (datum', script')))

instance SpecTranslate DijkstraEra (UTxO DijkstraEra) where
  type
    SpecRep DijkstraEra (UTxO DijkstraEra) =
      Agda.HSMap (SpecRep DijkstraEra TxIn) (SpecRep DijkstraEra (TxOut DijkstraEra))

  toSpecRep (UTxO m) = toSpecRepMap m

deriving instance SpecTranslate DijkstraEra OrdExUnits

deriving instance SpecTranslate DijkstraEra CoinPerByte

instance
  SpecTranslate DijkstraEra (HKD f a) =>
  SpecTranslate DijkstraEra (THKD r f a)
  where
  type SpecRep DijkstraEra (THKD r f a) = SpecRep DijkstraEra (HKD f a)
  type SpecContext DijkstraEra (THKD r f a) = SpecContext DijkstraEra (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate DijkstraEra DRepVotingThresholds where
  type SpecRep DijkstraEra DRepVotingThresholds = Agda.DrepThresholds

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

instance SpecTranslate DijkstraEra PoolVotingThresholds where
  type SpecRep DijkstraEra PoolVotingThresholds = Agda.PoolThresholds

  toSpecRep PoolVotingThresholds {..} =
    Agda.MkPoolThresholds
      <$> toSpecRep pvtMotionNoConfidence
      <*> toSpecRep pvtCommitteeNormal
      <*> toSpecRep pvtCommitteeNoConfidence
      <*> toSpecRep pvtHardForkInitiation
      <*> toSpecRep pvtPPSecurityGroup

instance SpecTranslate DijkstraEra (DijkstraPParams Identity DijkstraEra) where
  type SpecRep DijkstraEra (DijkstraPParams Identity DijkstraEra) = Agda.PParams

  toSpecRep dpp@DijkstraPParams {..} = do
    ppA <- toSpecRep dppTxFeePerByte
    ppB <- toSpecRep dppTxFeeFixed
    ppA0 <- toSpecRep dppA0
    ppMinFeeRefScriptCoinsPerByte <- toSpecRep dppMinFeeRefScriptCostPerByte
    ppCollateralPercentage <- toSpecRep dppCollateralPercentage
    let
      ppMaxBlockSize = toInteger $ unTHKD dppMaxBBSize
      ppMaxTxSize = toInteger $ unTHKD dppMaxTxSize
      ppMaxHeaderSize = toInteger $ unTHKD dppMaxBHSize
    ppKeyDeposit <- toSpecRep dppKeyDeposit
    ppPoolDeposit <- toSpecRep dppPoolDeposit
    ppEmax <- toSpecRep dppEMax
    ppNopt <- toSpecRep (toInteger $ unTHKD dppNOpt)
    let
      ppPv = (0, 0)
      ppMinUTxOValue = 0
    ppCoinsPerUTxOByte <- toSpecRep dppCoinsPerUTxOByte
    ppCostmdlsAssoc <- toSpecRep dppCostModels
    ppPrices <- toSpecRep dppPrices
    let
      pp = PParams dpp
      ppMaxRefScriptSizePerTx = toInteger $ pp ^. ppMaxRefScriptSizePerTxG
      ppMaxRefScriptSizePerBlock = toInteger $ pp ^. ppMaxRefScriptSizePerBlockG
      ppRefScriptCostStride = toInteger . unNonZero $ pp ^. ppRefScriptCostStrideG
      ppRefScriptCostMultiplier = unboundRational $ pp ^. ppRefScriptCostMultiplierG
    ppMaxTxExUnits <- toSpecRep dppMaxTxExUnits
    ppMaxBlockExUnits <- toSpecRep dppMaxBlockExUnits
    let
      ppMaxValSize = toInteger . unTHKD $ dppMaxValSize
      ppMaxCollateralInputs = toInteger . unTHKD $ dppMaxCollateralInputs
    ppPoolThresholds <- toSpecRep dppPoolVotingThresholds
    ppDrepThresholds <- toSpecRep dppDRepVotingThresholds
    let
      ppCcMinSize = toInteger . unTHKD $ dppCommitteeMinSize
      ppCcMaxTermLength = toInteger . unEpochInterval . unTHKD $ dppCommitteeMaxTermLength
    ppGovActionLifetime <- toSpecRep dppGovActionLifetime
    ppGovActionDeposit <- toSpecRep dppGovActionDeposit
    ppDrepDeposit <- toSpecRep dppDRepDeposit
    ppDrepActivity <- toSpecRep dppDRepActivity
    ppMonetaryExpansion <- toSpecRep dppRho
    ppTreasuryCut <- toSpecRep dppTau

    pure Agda.MkPParams {..}

instance SpecTranslate DijkstraEra ValidityInterval where
  type SpecRep DijkstraEra ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRepTuple (lo, hi)

instance SpecTranslate DijkstraEra (TxDats DijkstraEra) where
  type SpecRep DijkstraEra (TxDats DijkstraEra) = Agda.HSSet Agda.Datum

  toSpecRep = fmap Agda.MkHSSet . traverse (toSpecRep . snd) . Map.toList . unTxDats

instance SpecTranslate DijkstraEra (DijkstraPlutusPurpose AsIx DijkstraEra) where
  type SpecRep DijkstraEra (DijkstraPlutusPurpose AsIx DijkstraEra) = Agda.RedeemerPtr

  toSpecRep = \case
    DijkstraSpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    DijkstraMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    DijkstraCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    DijkstraWithdrawing (AsIx i) -> pure (Agda.Reward, toInteger i)
    DijkstraVoting (AsIx i) -> pure (Agda.Vote, toInteger i)
    DijkstraProposing (AsIx i) -> pure (Agda.Propose, toInteger i)
    DijkstraGuarding (AsIx i) -> pure (Agda.Guard, toInteger i)

instance SpecTranslate DijkstraEra (Redeemers DijkstraEra) where
  type
    SpecRep DijkstraEra (Redeemers DijkstraEra) =
      Agda.HSMap (SpecRep DijkstraEra (PlutusPurpose AsIx DijkstraEra)) (Agda.Redeemer, Agda.ExUnits)

  toSpecRep (Redeemers x) =
    fmap Agda.MkHSMap
      . traverse (toSpecRepTupleGen toSpecRep toSpecRepTuple)
      . Map.toList
      $ x

instance SpecTranslate DijkstraEra (AlonzoTxWits DijkstraEra) where
  type SpecRep DijkstraEra (AlonzoTxWits DijkstraEra) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> fmap Agda.MkHSMap (toSpecRep txWitsMap)
      <*> fmap Agda.MkHSSet (toSpecRep (Map.elems $ txscripts x))
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)
    where
      txWitsMap = toList (txwitsVKey x)

instance SpecTranslate DijkstraEra (AlonzoTxAuxData DijkstraEra) where
  type SpecRep DijkstraEra (AlonzoTxAuxData DijkstraEra) = Agda.AuxiliaryData

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate DijkstraEra StakePoolParams where
  type SpecRep DijkstraEra StakePoolParams = Agda.StakePoolParams

  toSpecRep StakePoolParams {..} =
    Agda.StakePoolParams
      <$> toSpecRep sppOwners
      <*> toSpecRep sppCost
      <*> toSpecRep sppMargin
      <*> toSpecRep sppPledge
      <*> toSpecRep (sppAccountAddress ^. accountAddressCredentialL)

instance SpecTranslate DijkstraEra DRep where
  type SpecRep DijkstraEra DRep = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.VDelegCredential <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.VDelegAbstain
  toSpecRep DRepAlwaysNoConfidence = pure Agda.VDelegNoConfidence

instance SpecTranslate DijkstraEra Url where
  type SpecRep DijkstraEra Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate DijkstraEra Anchor where
  type SpecRep DijkstraEra Anchor = Agda.Anchor
  toSpecRep (Anchor url h) = Agda.Anchor <$> toSpecRep url <*> toSpecRep h

instance SpecTranslate DijkstraEra Withdrawals where
  type SpecRep DijkstraEra Withdrawals = Agda.Withdrawals

  toSpecRep (Withdrawals w) = toSpecRepMap w

instance SpecTranslate DijkstraEra IsPhase2Valid where
  type SpecRep DijkstraEra IsPhase2Valid = Bool

  toSpecRep Phase2Valid = pure True
  toSpecRep Phase2Invalid = pure False

instance SpecTranslate DijkstraEra (GovPurposeId r) where
  type SpecRep DijkstraEra (GovPurposeId r) = (Agda.TxId, Integer)

  toSpecRep (GovPurposeId gaId) = toSpecRep gaId

instance SpecTranslate DijkstraEra (Committee DijkstraEra) where
  type
    SpecRep DijkstraEra (Committee DijkstraEra) =
      (Agda.HSMap Agda.Credential Agda.Epoch, Agda.Rational)

  toSpecRep (Committee members threshold) = (,) <$> toSpecRepMap members <*> toSpecRep threshold

instance SpecTranslate DijkstraEra (Constitution DijkstraEra) where
  type SpecRep DijkstraEra (Constitution DijkstraEra) = (Agda.DataHash, Maybe Agda.ScriptHash)

  toSpecRep (Constitution (Anchor _ h) policy) = toSpecRepTuple (h, policy)

instance SpecTranslate DijkstraEra (EnactState DijkstraEra) where
  type SpecRep DijkstraEra (EnactState DijkstraEra) = Agda.EnactState

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
          network <- toSpecRep Testnet
          pure (Agda.RewardAddress network agdaCred, amount)
      transHashProtected ::
        (SpecTranslate DijkstraEra a, SpecContext DijkstraEra a ~ ()) =>
        a ->
        StrictMaybe (GovPurposeId p) ->
        SpecTransM DijkstraEra () (SpecRep DijkstraEra a, (Integer, Integer))
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance SpecTranslate DijkstraEra Voter where
  type SpecRep DijkstraEra Voter = Agda.GovVoter

  toSpecRep (CommitteeVoter c) = (Agda.CC,) <$> toSpecRep c
  toSpecRep (DRepVoter c) = (Agda.DRep,) <$> toSpecRep c
  toSpecRep (StakePoolVoter kh) = (Agda.SPO,) <$> toSpecRep (KeyHashObj kh)

instance SpecTranslate DijkstraEra Vote where
  type SpecRep DijkstraEra Vote = Agda.Vote

  toSpecRep VoteYes = pure Agda.Yes
  toSpecRep VoteNo = pure Agda.No
  toSpecRep Abstain = pure Agda.Abstain

instance SpecTranslate DijkstraEra (VotingProcedures DijkstraEra) where
  type SpecRep DijkstraEra (VotingProcedures DijkstraEra) = [Agda.GovVote]

  toSpecRep = foldrVotingProcedures go (pure [])
    where
      go ::
        Voter ->
        GovActionId ->
        VotingProcedure DijkstraEra ->
        SpecTransM DijkstraEra () [Agda.GovVote] ->
        SpecTransM DijkstraEra () [Agda.GovVote]
      go voter gaId votingProcedure m =
        (:)
          <$> ( Agda.MkGovVote
                  <$> toSpecRep gaId
                  <*> toSpecRep voter
                  <*> toSpecRep (vProcVote votingProcedure)
                  <*> toSpecRep (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate DijkstraEra (DijkstraPParams StrictMaybe DijkstraEra) where
  type SpecRep DijkstraEra (DijkstraPParams StrictMaybe DijkstraEra) = Agda.PParamsUpdate

  toSpecRep (DijkstraPParams {..}) = do
    ppuA <- toSpecRep dppTxFeePerByte
    ppuB <- toSpecRep dppTxFeeFixed
    ppuA0 <- toSpecRep dppA0
    ppuMinFeeRefScriptCoinsPerByte <- toSpecRep dppMinFeeRefScriptCostPerByte
    ppuCollateralPercentage <- toSpecRep dppCollateralPercentage
    let
      ppuMaxBlockSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ dppMaxBBSize
      ppuMaxTxSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ dppMaxTxSize
      ppuMaxHeaderSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ dppMaxBHSize
    ppuKeyDeposit <- toSpecRep dppKeyDeposit
    ppuPoolDeposit <- toSpecRep dppPoolDeposit
    ppuEmax <- toSpecRep dppEMax
    ppuNopt <- toSpecRep (fmap toInteger . strictMaybeToMaybe $ unTHKD dppNOpt)
    let
      ppuPv = Nothing
      ppuMinUTxOValue = Nothing
    ppuCoinsPerUTxOByte <- toSpecRep dppCoinsPerUTxOByte
    ppuCostmdls <- toSpecRep dppCostModels
    ppuPrices <- toSpecRep dppPrices
    let
      ppuMaxRefScriptSizePerTx = Nothing
      ppuMaxRefScriptSizePerBlock = Nothing
      ppuRefScriptCostStride = Nothing
      ppuRefScriptCostMultiplier = Nothing
    ppuMaxTxExUnits <- toSpecRep dppMaxTxExUnits
    ppuMaxBlockExUnits <- toSpecRep dppMaxBlockExUnits
    let
      ppuMaxValSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ dppMaxValSize
      ppuMaxCollateralInputs = fmap toInteger . strictMaybeToMaybe . unTHKD $ dppMaxCollateralInputs
    ppuPoolThresholds <- toSpecRep dppPoolVotingThresholds
    ppuDrepThresholds <- toSpecRep dppDRepVotingThresholds
    let
      ppuCcMinSize = fmap toInteger . strictMaybeToMaybe $ unTHKD dppCommitteeMinSize
      ppuCcMaxTermLength =
        fmap (toInteger . unEpochInterval) . strictMaybeToMaybe $ unTHKD dppCommitteeMaxTermLength
    ppuGovActionLifetime <- toSpecRep dppGovActionLifetime
    ppuGovActionDeposit <- toSpecRep dppGovActionDeposit
    ppuDrepDeposit <- toSpecRep dppDRepDeposit
    ppuDrepActivity <- toSpecRep dppDRepActivity
    ppuMonetaryExpansion <- toSpecRep dppRho
    ppuTreasuryCut <- toSpecRep dppTau

    pure Agda.MkPParamsUpdate {..}

instance SpecTranslate DijkstraEra (GovAction DijkstraEra) where
  type SpecRep DijkstraEra (GovAction DijkstraEra) = Agda.GovAction

  toSpecRep (ParameterChange _ ppu _) = Agda.ChangePParams <$> toSpecRep ppu
  toSpecRep (HardForkInitiation _ pv) = Agda.TriggerHardFork <$> toSpecRep pv
  toSpecRep (TreasuryWithdrawals withdrawals _) =
    Agda.TreasuryWithdrawal
      <$> toSpecRepMap withdrawals
  toSpecRep (NoConfidence _) = pure Agda.NoConfidence
  toSpecRep (UpdateCommittee _ remove add threshold) =
    Agda.UpdateCommittee
      <$> toSpecRepMap add
      <*> toSpecRep remove
      <*> toSpecRep threshold
  toSpecRep (NewConstitution _ (Constitution (Anchor _ h) policy)) =
    Agda.NewConstitution
      <$> toSpecRep h
      <*> toSpecRep policy
  toSpecRep InfoAction = pure Agda.Info

instance SpecTranslate DijkstraEra (ProposalProcedure DijkstraEra) where
  type SpecRep DijkstraEra (ProposalProcedure DijkstraEra) = Agda.GovProposal

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

instance SpecTranslate DijkstraEra (GovActionState DijkstraEra) where
  type SpecRep DijkstraEra (GovActionState DijkstraEra) = Agda.GovActionState

  toSpecRep gas@GovActionState {..} = do
    Agda.MkGovActionState
      <$> ( Agda.GovVotes
              <$> toSpecRepMap gasCommitteeVotes
              <*> toSpecRepMap gasDRepVotes
              <*> toSpecRepMap gasStakePoolVotes
          )
      <*> toSpecRep (gasReturnAddr gas)
      <*> toSpecRep gasExpiresAfter
      <*> toSpecRep action
      <*> toSpecRep (nullifyIfNotNeeded (prevGovActionId action) action)
      <*> toSpecRep (gasDeposit gas)
    where
      action = gasAction gas

instance SpecTranslate DijkstraEra GovActionIx where
  type SpecRep DijkstraEra GovActionIx = Integer

  toSpecRep = pure . fromIntegral . unGovActionIx

instance SpecTranslate DijkstraEra GovActionId where
  type SpecRep DijkstraEra GovActionId = Agda.GovActionID

  toSpecRep (GovActionId txId gaIx) = toSpecRepTuple (txId, gaIx)

instance SpecTranslate DijkstraEra (Proposals DijkstraEra) where
  type SpecRep DijkstraEra (Proposals DijkstraEra) = Agda.GovState

  toSpecRep = toSpecRepOMap . prioritySort . view pPropsL
    where
      prioritySort ::
        OMap GovActionId (GovActionState DijkstraEra) ->
        OMap GovActionId (GovActionState DijkstraEra)
      prioritySort = Exts.fromList . sortOn (actionPriority . gasAction) . Exts.toList

instance SpecTranslate DijkstraEra MaryValue where
  type SpecRep DijkstraEra MaryValue = Agda.Coin

  toSpecRep = toSpecRep . coin

instance SpecTranslate DijkstraEra (RatifyState DijkstraEra) where
  type SpecRep DijkstraEra (RatifyState DijkstraEra) = Agda.RatifyState

  type SpecContext DijkstraEra (RatifyState DijkstraEra) = [GovActionState DijkstraEra]
  toSpecRep RatifyState {..} = do
    govActionMap <-
      foldl' (\acc gas -> Map.insert (gasId gas) gas acc) mempty
        <$> askSpecTransM
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
    withCtxSpecTransM () $ do
      Agda.MkRatifyState
        <$> toSpecRep rsEnactState
        <*> (Agda.MkHSSet <$> traverse toSpecRepTuple (toList removed))
        <*> toSpecRep rsDelayed

instance SpecTranslate DijkstraEra (RatifySignal DijkstraEra) where
  type
    SpecRep DijkstraEra (RatifySignal DijkstraEra) =
      [(SpecRep DijkstraEra GovActionId, SpecRep DijkstraEra (GovActionState DijkstraEra))]
  toSpecRep (RatifySignal x) =
    traverse (\gas@GovActionState {gasId} -> toSpecRepTuple (gasId, gas)) (toList x)

instance SpecTranslate DijkstraEra (Conway.EnactSignal DijkstraEra) where
  type
    SpecRep DijkstraEra (Conway.EnactSignal DijkstraEra) =
      SpecRep DijkstraEra (GovAction DijkstraEra)

  toSpecRep (Conway.EnactSignal _ ga) = toSpecRep ga

instance SpecNormalize Agda.NativeScript

instance SpecNormalize Agda.HSNativeScript

instance SpecNormalize Agda.LanguageCostModels where
  specNormalize = Agda.MkLanguageCostModels . sortOn fst . Agda.lcmLanguageCostModels

instance SpecNormalize Agda.HSLanguage

instance SpecNormalize Agda.HSPlutusScript

instance SpecNormalize Agda.UTxOState

instance SpecNormalize Agda.GovRole

instance SpecNormalize Agda.GovVotes

instance SpecNormalize Agda.VDeleg

instance SpecNormalize Agda.DState

instance SpecNormalize Agda.StakePoolParams

instance SpecNormalize Agda.PState

instance SpecNormalize Agda.GState

instance SpecNormalize Agda.CertState

instance SpecNormalize Agda.Vote

instance SpecNormalize Agda.PParamsUpdate

instance SpecNormalize Agda.GovAction

instance SpecNormalize Agda.GovActionState

instance SpecNormalize Agda.StakeDistrs

instance SpecNormalize Agda.PoolThresholds

instance SpecNormalize Agda.DrepThresholds

instance SpecNormalize Agda.PParams

instance SpecNormalize Agda.EnactState

instance SpecNormalize Agda.RatifyEnv

instance SpecNormalize Agda.RatifyState

instance SpecNormalize Agda.EpochState

instance SpecNormalize Agda.Snapshots

instance SpecNormalize Agda.Snapshot where
  specNormalize (Agda.MkSnapshot s d p) =
    Agda.MkSnapshot (specNormalize s') (specNormalize d') p
    where
      s' = removeZero s
      -- Only keep delegations for credentials that have non-zero stake,
      -- since ActiveStake drops zero-stake credentials
      d' = keepOnlyStaked s' (removeZero d)
      removeZero (Agda.MkHSMap l) = Agda.MkHSMap $ filter ((/= 0) . snd) l
      keepOnlyStaked (Agda.MkHSMap sl) (Agda.MkHSMap dl) =
        let stakeKeys = Set.fromList (map fst sl)
         in Agda.MkHSMap $ filter ((`Set.member` stakeKeys) . fst) dl

instance SpecNormalize Agda.Acnt

instance SpecNormalize Agda.LedgerState

instance SpecNormalize Agda.LedgerEnv

instance SpecNormalize Agda.RewardUpdate

instance SpecNormalize Agda.NewEpochState

instance SpecNormalize Agda.BalanceInterval

instance SpecNormalize Agda.Anchor
