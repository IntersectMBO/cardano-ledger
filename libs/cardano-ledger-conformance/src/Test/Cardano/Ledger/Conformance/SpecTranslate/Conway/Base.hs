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

import Cardano.Ledger.Address (accountAddressCredentialL)
import Cardano.Ledger.Allegra.Scripts (
  Timelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (AlonzoTxAuxData, MaryValue)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..), plutusScriptLanguage)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..), unTxDats)
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..), ConwayPParams (..), THKD (..))
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..), ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
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
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance.Orphans.Conway ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (committeeCredentialToStrictMaybe)
import Test.Cardano.Ledger.Conway.TreeDiff (showExpr)

instance SpecTranslate ConwayEra TxId where
  type SpecRep ConwayEra TxId = Agda.TxId

  toSpecRep (TxId x) = toSpecRep x

instance SpecTranslate ConwayEra TxIn where
  type SpecRep ConwayEra TxIn = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRepTuple (txId, txIx)

instance SpecTranslate ConwayEra (SafeHash a) where
  type SpecRep ConwayEra (SafeHash a) = Agda.DataHash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate ConwayEra Language where
  type SpecRep ConwayEra Language = Agda.HSLanguage

  toSpecRep l = case l of
    PlutusV1 -> return Agda.PV1
    PlutusV2 -> return Agda.PV2
    PlutusV3 -> return Agda.PV3
    PlutusV4 -> error "PlutusV4 not supported"

instance SpecTranslate ConwayEra CostModels where
  type SpecRep ConwayEra CostModels = Agda.LanguageCostModels

  toSpecRep cm =
    -- filter out PlutusV4 language
    let validCostModels = filter ((/= PlutusV4) . fst) $ Map.toList (costModelsValid cm)
     in Agda.MkLanguageCostModels <$> mapM (\(l, _) -> (,()) <$> toSpecRep l) validCostModels

instance SpecTranslate ConwayEra ExUnits where
  type SpecRep ConwayEra ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

instance SpecTranslate ConwayEra (BinaryData ConwayEra) where
  type SpecRep ConwayEra (BinaryData ConwayEra) = Agda.DataHash

  toSpecRep = toSpecRep . hashBinaryData

instance SpecTranslate ConwayEra (Datum ConwayEra) where
  type SpecRep ConwayEra (Datum ConwayEra) = Maybe (Either Agda.Datum Agda.DataHash)

  toSpecRep NoDatum = pure Nothing
  toSpecRep (Datum d) = Just . Left <$> toSpecRep d
  toSpecRep (DatumHash h) = Just . Right <$> toSpecRep h

instance SpecTranslate ConwayEra (Data ConwayEra) where
  type SpecRep ConwayEra (Data ConwayEra) = Agda.DataHash

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate ConwayEra TxAuxDataHash where
  type SpecRep ConwayEra TxAuxDataHash = Agda.DataHash

  toSpecRep (TxAuxDataHash x) = toSpecRep x

instance SpecTranslate ConwayEra (Timelock ConwayEra) where
  type SpecRep ConwayEra (Timelock ConwayEra) = Agda.HSTimelock

  toSpecRep tl =
    Agda.HSTimelock
      <$> timelockToSpecRep tl
      <*> toSpecRep (hashScript $ NativeScript tl)
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

instance SpecTranslate ConwayEra (PlutusScript ConwayEra) where
  type SpecRep ConwayEra (PlutusScript ConwayEra) = Agda.HSPlutusScript

  toSpecRep ps =
    Agda.MkHSPlutusScript
      <$> toSpecRep (hashScript $ PlutusScript ps)
      <*> pure (fromIntegral $ originalBytesSize ps)
      <*> toSpecRep (plutusScriptLanguage ps)

instance SpecTranslate ConwayEra (AlonzoScript ConwayEra) where
  type SpecRep ConwayEra (AlonzoScript ConwayEra) = Agda.Script

  toSpecRep (NativeScript s) = Left <$> toSpecRep s
  toSpecRep (PlutusScript s) = Right <$> toSpecRep s

instance SpecTranslate ConwayEra (BabbageTxOut ConwayEra) where
  type SpecRep ConwayEra (BabbageTxOut ConwayEra) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum script) = do
    addr' <- toSpecRep addr
    val' <- toSpecRep val
    datum' <- toSpecRep datum
    script' <- toSpecRep script
    pure (addr', (val', (datum', script')))

instance SpecTranslate ConwayEra (UTxO ConwayEra) where
  type
    SpecRep ConwayEra (UTxO ConwayEra) =
      Agda.HSMap (SpecRep ConwayEra TxIn) (SpecRep ConwayEra (TxOut ConwayEra))

  toSpecRep (UTxO m) = toSpecRepMap m

deriving instance SpecTranslate ConwayEra OrdExUnits

deriving instance SpecTranslate ConwayEra CoinPerByte

instance
  SpecTranslate ConwayEra (HKD f a) =>
  SpecTranslate ConwayEra (THKD r f a)
  where
  type SpecRep ConwayEra (THKD r f a) = SpecRep ConwayEra (HKD f a)
  type SpecContext ConwayEra (THKD r f a) = SpecContext ConwayEra (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate ConwayEra DRepVotingThresholds where
  type SpecRep ConwayEra DRepVotingThresholds = Agda.DrepThresholds

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

instance SpecTranslate ConwayEra PoolVotingThresholds where
  type SpecRep ConwayEra PoolVotingThresholds = Agda.PoolThresholds

  toSpecRep PoolVotingThresholds {..} =
    Agda.MkPoolThresholds
      <$> toSpecRep pvtMotionNoConfidence
      <*> toSpecRep pvtCommitteeNormal
      <*> toSpecRep pvtCommitteeNoConfidence
      <*> toSpecRep pvtHardForkInitiation
      <*> toSpecRep pvtPPSecurityGroup

instance SpecTranslate ConwayEra (ConwayPParams Identity ConwayEra) where
  type SpecRep ConwayEra (ConwayPParams Identity ConwayEra) = Agda.PParams

  toSpecRep cpp@ConwayPParams {..} = do
    ppA <- toSpecRep cppTxFeePerByte
    ppB <- toSpecRep cppTxFeeFixed
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
    ppCostmdlsAssoc <- toSpecRep cppCostModels
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

instance SpecTranslate ConwayEra ValidityInterval where
  type SpecRep ConwayEra ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRepTuple (lo, hi)

instance SpecTranslate ConwayEra (TxDats ConwayEra) where
  type SpecRep ConwayEra (TxDats ConwayEra) = Agda.HSSet Agda.Datum

  toSpecRep = fmap Agda.MkHSSet . traverse (toSpecRep . snd) . Map.toList . unTxDats

instance SpecTranslate ConwayEra (AlonzoPlutusPurpose AsIx ConwayEra) where
  type SpecRep ConwayEra (AlonzoPlutusPurpose AsIx ConwayEra) = Agda.RdmrPtr

  toSpecRep = \case
    AlonzoSpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    AlonzoMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    AlonzoCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    AlonzoWithdrawing (AsIx i) -> pure (Agda.Rewrd, toInteger i)

instance SpecTranslate ConwayEra (ConwayPlutusPurpose AsIx ConwayEra) where
  type SpecRep ConwayEra (ConwayPlutusPurpose AsIx ConwayEra) = Agda.RdmrPtr

  toSpecRep = \case
    ConwaySpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    ConwayMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    ConwayCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    ConwayRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)
    ConwayVoting (AsIx i) -> pure (Agda.Vote, toInteger i)
    ConwayProposing (AsIx i) -> pure (Agda.Propose, toInteger i)

instance SpecTranslate ConwayEra (Redeemers ConwayEra) where
  type
    SpecRep ConwayEra (Redeemers ConwayEra) =
      Agda.HSMap (SpecRep ConwayEra (PlutusPurpose AsIx ConwayEra)) (Agda.Redeemer, Agda.ExUnits)

  toSpecRep (Redeemers x) =
    fmap Agda.MkHSMap
      . traverse (toSpecRepTupleGen toSpecRep toSpecRepTuple)
      . Map.toList
      $ x

instance SpecTranslate ConwayEra (AlonzoTxWits ConwayEra) where
  type SpecRep ConwayEra (AlonzoTxWits ConwayEra) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> fmap Agda.MkHSMap (toSpecRep txWitsMap)
      <*> fmap Agda.MkHSSet (toSpecRep (Map.elems $ txscripts x))
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)
    where
      txWitsMap = toList (txwitsVKey x)

instance SpecTranslate ConwayEra (AlonzoTxAuxData ConwayEra) where
  type SpecRep ConwayEra (AlonzoTxAuxData ConwayEra) = Agda.AuxiliaryData

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate ConwayEra StakePoolParams where
  type SpecRep ConwayEra StakePoolParams = Agda.StakePoolParams

  toSpecRep StakePoolParams {..} =
    Agda.StakePoolParams
      <$> toSpecRep sppOwners
      <*> toSpecRep sppCost
      <*> toSpecRep sppMargin
      <*> toSpecRep sppPledge
      <*> toSpecRep (sppAccountAddress ^. accountAddressCredentialL)

instance SpecTranslate ConwayEra DRep where
  type SpecRep ConwayEra DRep = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.VDelegCredential <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.VDelegAbstain
  toSpecRep DRepAlwaysNoConfidence = pure Agda.VDelegNoConfidence

instance SpecTranslate ConwayEra Url where
  type SpecRep ConwayEra Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate ConwayEra Anchor where
  type SpecRep ConwayEra Anchor = Agda.Anchor
  toSpecRep (Anchor url h) = Agda.Anchor <$> toSpecRep url <*> toSpecRep h

instance SpecTranslate ConwayEra Withdrawals where
  type SpecRep ConwayEra Withdrawals = Agda.Withdrawals

  toSpecRep (Withdrawals w) = toSpecRepMap w

instance SpecTranslate ConwayEra IsValid where
  type SpecRep ConwayEra IsValid = Bool

  toSpecRep (IsValid b) = pure b

instance SpecTranslate ConwayEra (GovPurposeId r) where
  type SpecRep ConwayEra (GovPurposeId r) = (Agda.TxId, Integer)

  toSpecRep (GovPurposeId gaId) = toSpecRep gaId

instance SpecTranslate ConwayEra (Committee ConwayEra) where
  type
    SpecRep ConwayEra (Committee ConwayEra) =
      (Agda.HSMap Agda.Credential Agda.Epoch, Agda.Rational)

  toSpecRep (Committee members threshold) = (,) <$> toSpecRepMap members <*> toSpecRep threshold

instance SpecTranslate ConwayEra (Constitution ConwayEra) where
  type SpecRep ConwayEra (Constitution ConwayEra) = (Agda.DataHash, Maybe Agda.ScriptHash)

  toSpecRep (Constitution (Anchor _ h) policy) = toSpecRepTuple (h, policy)

instance SpecTranslate ConwayEra (EnactState ConwayEra) where
  type SpecRep ConwayEra (EnactState ConwayEra) = Agda.EnactState

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
          pure (Agda.RewardAddress network agdaCred, amount)
      transHashProtected ::
        (SpecTranslate ConwayEra a, SpecContext ConwayEra a ~ ()) =>
        a ->
        StrictMaybe (GovPurposeId p) ->
        SpecTransM ConwayEra () (SpecRep ConwayEra a, (Integer, Integer))
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance SpecTranslate ConwayEra Voter where
  type SpecRep ConwayEra Voter = Agda.GovVoter

  toSpecRep (CommitteeVoter c) = (Agda.CC,) <$> toSpecRep c
  toSpecRep (DRepVoter c) = (Agda.DRep,) <$> toSpecRep c
  toSpecRep (StakePoolVoter kh) = (Agda.SPO,) <$> toSpecRep (KeyHashObj kh)

instance SpecTranslate ConwayEra Vote where
  type SpecRep ConwayEra Vote = Agda.Vote

  toSpecRep VoteYes = pure Agda.Yes
  toSpecRep VoteNo = pure Agda.No
  toSpecRep Abstain = pure Agda.Abstain

instance SpecTranslate ConwayEra (VotingProcedures ConwayEra) where
  type SpecRep ConwayEra (VotingProcedures ConwayEra) = [Agda.GovVote]

  toSpecRep = foldrVotingProcedures go (pure [])
    where
      go ::
        Voter ->
        GovActionId ->
        VotingProcedure ConwayEra ->
        SpecTransM ConwayEra () [Agda.GovVote] ->
        SpecTransM ConwayEra () [Agda.GovVote]
      go voter gaId votingProcedure m =
        (:)
          <$> ( Agda.MkGovVote
                  <$> toSpecRep gaId
                  <*> toSpecRep voter
                  <*> toSpecRep (vProcVote votingProcedure)
                  <*> toSpecRep (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate ConwayEra (ConwayPParams StrictMaybe ConwayEra) where
  type SpecRep ConwayEra (ConwayPParams StrictMaybe ConwayEra) = Agda.PParamsUpdate

  toSpecRep (ConwayPParams {..}) = do
    ppuA <- toSpecRep cppTxFeePerByte
    ppuB <- toSpecRep cppTxFeeFixed
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

instance SpecTranslate ConwayEra (GovAction ConwayEra) where
  type SpecRep ConwayEra (GovAction ConwayEra) = Agda.GovAction

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

instance SpecTranslate ConwayEra (ProposalProcedure ConwayEra) where
  type SpecRep ConwayEra (ProposalProcedure ConwayEra) = Agda.GovProposal

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

instance SpecTranslate ConwayEra (GovActionState ConwayEra) where
  type SpecRep ConwayEra (GovActionState ConwayEra) = Agda.GovActionState

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
    where
      action = gasAction gas

instance SpecTranslate ConwayEra GovActionIx where
  type SpecRep ConwayEra GovActionIx = Integer

  toSpecRep = pure . fromIntegral . unGovActionIx

instance SpecTranslate ConwayEra GovActionId where
  type SpecRep ConwayEra GovActionId = Agda.GovActionID

  toSpecRep (GovActionId txId gaIx) = toSpecRepTuple (txId, gaIx)

instance SpecTranslate ConwayEra (Proposals ConwayEra) where
  type SpecRep ConwayEra (Proposals ConwayEra) = Agda.GovState

  -- TODO get rid of `prioritySort` once we've changed the implementation so
  -- that the proposals are always sorted
  toSpecRep = toSpecRepOMap . prioritySort . view pPropsL
    where
      prioritySort ::
        OMap GovActionId (GovActionState ConwayEra) ->
        OMap GovActionId (GovActionState ConwayEra)
      prioritySort = Exts.fromList . sortOn (actionPriority . gasAction) . Exts.toList

instance SpecTranslate ConwayEra MaryValue where
  type SpecRep ConwayEra MaryValue = Agda.Coin

  toSpecRep = toSpecRep . coin

instance SpecTranslate ConwayEra (RatifyEnv ConwayEra) where
  type SpecRep ConwayEra (RatifyEnv ConwayEra) = Agda.RatifyEnv
  type SpecContext ConwayEra (RatifyEnv ConwayEra) = Coin

  toSpecRep RatifyEnv {..} = do
    let
      stakeDistrs =
        Agda.StakeDistrs
          <$> toSpecRepMap reDRepDistr
          <*> toSpecRep reStakePoolDistr
      dreps = toSpecRepMap $ Map.map drepExpiry reDRepState
    treasury <- askSpecTransM
    withCtxSpecTransM () $ do
      Agda.MkRatifyEnv
        <$> stakeDistrs
        <*> toSpecRep reCurrentEpoch
        <*> dreps
        <*> toSpecRep reCommitteeState
        <*> toSpecRep treasury
        <*> toSpecRepMap (Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) reStakePools)
        <*> toSpecRepMap (Map.mapMaybe (view dRepDelegationAccountStateL) (reAccounts ^. accountsMapL))

instance SpecTranslate ConwayEra (RatifyState ConwayEra) where
  type SpecRep ConwayEra (RatifyState ConwayEra) = Agda.RatifyState
  type SpecContext ConwayEra (RatifyState ConwayEra) = [GovActionState ConwayEra]

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

instance SpecTranslate ConwayEra (RatifySignal ConwayEra) where
  type
    SpecRep ConwayEra (RatifySignal ConwayEra) =
      [(SpecRep ConwayEra GovActionId, SpecRep ConwayEra (GovActionState ConwayEra))]

  toSpecRep (RatifySignal x) =
    traverse (\gas@GovActionState {gasId} -> toSpecRepTuple (gasId, gas)) (toList x)

instance SpecTranslate ConwayEra (Conway.EnactSignal ConwayEra) where
  type SpecRep ConwayEra (Conway.EnactSignal ConwayEra) = SpecRep ConwayEra (GovAction ConwayEra)

  toSpecRep (Conway.EnactSignal _ ga) = toSpecRep ga

instance SpecNormalize TxId where
  specNormalize = id

instance SpecNormalize Agda.Timelock

instance SpecNormalize Agda.HSTimelock

instance SpecNormalize Agda.LanguageCostModels where
  specNormalize = Agda.MkLanguageCostModels . sortOn fst . Agda.lcmLanguageCostModels

instance SpecNormalize Agda.HSLanguage

instance SpecNormalize Agda.HSPlutusScript

instance SpecNormalize Agda.UTxOState

instance SpecNormalize Agda.GovRole

instance SpecNormalize Agda.GovVotes

instance SpecNormalize Agda.VDeleg

instance SpecNormalize Agda.DepositPurpose

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

instance SpecNormalize Agda.LState

instance SpecNormalize Agda.HsRewardUpdate

instance SpecNormalize Agda.NewEpochState
