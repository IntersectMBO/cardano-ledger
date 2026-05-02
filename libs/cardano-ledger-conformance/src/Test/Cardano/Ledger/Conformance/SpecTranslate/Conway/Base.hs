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
import Cardano.Ledger.Conway.Rules (EnactSignal (..))
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..), ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Plutus.CostModels (CostModels, costModelsValid)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum (..), hashBinaryData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (..))
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

instance SpecTranslate ctx ConwayEra TxId where
  type SpecRep ConwayEra TxId = Agda.TxId

  toSpecRep (TxId x) = toSpecRep @ctx @ConwayEra x

instance SpecTranslate ctx era TxIx where
  type SpecRep era TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate ctx ConwayEra TxIn where
  type SpecRep ConwayEra TxIn = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep @ctx @ConwayEra (txId, txIx)

instance SpecTranslate ctx ConwayEra (SafeHash a) where
  type SpecRep ConwayEra (SafeHash a) = Agda.DataHash

  toSpecRep = toSpecRep @ctx @ConwayEra . extractHash

instance SpecTranslate ctx ConwayEra Language where
  type SpecRep ConwayEra Language = Agda.HSLanguage

  toSpecRep l = case l of
    PlutusV1 -> return Agda.PV1
    PlutusV2 -> return Agda.PV2
    PlutusV3 -> return Agda.PV3
    PlutusV4 -> error "PlutusV4 not supported"

instance SpecTranslate ctx ConwayEra CostModels where
  type SpecRep ConwayEra CostModels = Agda.LanguageCostModels

  toSpecRep cm =
    -- filter out PlutusV4 language
    let validCostModels = filter ((/= PlutusV4) . fst) $ Map.toList (costModelsValid cm)
     in Agda.MkLanguageCostModels
          <$> mapM (\(l, _) -> (,()) <$> toSpecRep @ctx @ConwayEra l) validCostModels

instance SpecTranslate ctx ConwayEra ExUnits where
  type SpecRep ConwayEra ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

instance
  ( SpecRep ConwayEra DataHash ~ Agda.DataHash
  , Era era
  ) =>
  SpecTranslate ctx ConwayEra (BinaryData era)
  where
  type SpecRep ConwayEra (BinaryData era) = Agda.DataHash

  toSpecRep = toSpecRep @ctx @ConwayEra . hashBinaryData

instance Era era => SpecTranslate ctx ConwayEra (Datum era) where
  type SpecRep ConwayEra (Datum era) = Maybe (Either Agda.Datum Agda.DataHash)

  toSpecRep NoDatum = pure Nothing
  toSpecRep (Datum d) = Just . Left <$> toSpecRep @_ @ConwayEra d
  toSpecRep (DatumHash h) = Just . Right <$> toSpecRep @ctx @ConwayEra h

instance Era era => SpecTranslate ctx ConwayEra (Data era) where
  type SpecRep ConwayEra (Data era) = Agda.DataHash

  toSpecRep = toSpecRep @ctx @ConwayEra . hashAnnotated

instance SpecTranslate ctx ConwayEra TxAuxDataHash where
  type SpecRep ConwayEra TxAuxDataHash = Agda.DataHash

  toSpecRep (TxAuxDataHash x) = toSpecRep @ctx @ConwayEra x

instance
  ( AlonzoEraScript era
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate ctx ConwayEra (Timelock era)
  where
  type SpecRep ConwayEra (Timelock era) = Agda.HSTimelock

  toSpecRep tl =
    Agda.HSTimelock
      <$> timelockToSpecRep tl
      <*> toSpecRep @_ @ConwayEra (hashScript $ NativeScript tl)
      <*> pure (fromIntegral $ originalBytesSize tl)
    where
      timelockToSpecRep x =
        case x of
          RequireSignature kh ->
            Agda.RequireSig <$> toSpecRep @_ @ConwayEra kh
          RequireAllOf ss -> do
            tls <- traverse timelockToSpecRep ss
            pure . Agda.RequireAllOf $ toList tls
          RequireAnyOf ss -> do
            tls <- traverse timelockToSpecRep ss
            pure . Agda.RequireAnyOf $ toList tls
          RequireMOf m ss -> do
            tls <- traverse timelockToSpecRep ss
            pure . Agda.RequireMOf (toInteger m) $ toList tls
          RequireTimeExpire slot -> Agda.RequireTimeExpire <$> toSpecRep @_ @ConwayEra slot
          RequireTimeStart slot -> Agda.RequireTimeStart <$> toSpecRep @_ @ConwayEra slot
          _ -> error "Impossible: All NativeScripts should have been accounted for"

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate ctx ConwayEra (PlutusScript era)
  where
  type SpecRep ConwayEra (PlutusScript era) = Agda.HSPlutusScript

  toSpecRep ps =
    Agda.MkHSPlutusScript
      <$> toSpecRep @_ @ConwayEra (hashScript $ PlutusScript ps)
      <*> pure (fromIntegral $ originalBytesSize ps)
      <*> toSpecRep @_ @ConwayEra (plutusScriptLanguage ps)

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate ctx ConwayEra (AlonzoScript era)
  where
  type SpecRep ConwayEra (AlonzoScript era) = Agda.Script

  toSpecRep (NativeScript s) = Left <$> toSpecRep @_ @ConwayEra s
  toSpecRep (PlutusScript s) = Right <$> toSpecRep @_ @ConwayEra s

instance
  ( EraTxOut era
  , SpecRep ConwayEra (Value era) ~ Agda.Coin
  , Script era ~ AlonzoScript era
  , SpecTranslate ctx ConwayEra (Value era)
  , SpecTranslate ctx ConwayEra (Script era)
  ) =>
  SpecTranslate ctx ConwayEra (BabbageTxOut era)
  where
  type SpecRep ConwayEra (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum script) = do
    addr' <- toSpecRep @_ @ConwayEra addr
    val' <- toSpecRep @_ @ConwayEra val
    datum' <- toSpecRep @_ @ConwayEra datum
    script' <- toSpecRep @_ @ConwayEra script
    pure (addr', (val', (datum', script')))

instance
  ( SpecTranslate ctx ConwayEra (TxOut era)
  , SpecRep ConwayEra (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate ctx ConwayEra (UTxO era)
  where
  type SpecRep ConwayEra (UTxO era) = SpecRep ConwayEra (Map TxIn (TxOut era))

  toSpecRep (UTxO m) = toSpecRep @_ @ConwayEra m

deriving instance SpecTranslate ctx ConwayEra OrdExUnits

deriving instance SpecTranslate ctx ConwayEra CoinPerByte

instance
  SpecTranslate ctx ConwayEra (HKD f a) =>
  SpecTranslate ctx ConwayEra (THKD r f a)
  where
  type SpecRep ConwayEra (THKD r f a) = SpecRep ConwayEra (HKD f a)

  toSpecRep = toSpecRep @_ @ConwayEra . unTHKD

instance SpecTranslate ctx ConwayEra DRepVotingThresholds where
  type SpecRep ConwayEra DRepVotingThresholds = Agda.DrepThresholds

  toSpecRep DRepVotingThresholds {..} =
    Agda.MkDrepThresholds
      <$> toSpecRep @_ @ConwayEra dvtMotionNoConfidence
      <*> toSpecRep @_ @ConwayEra dvtCommitteeNormal
      <*> toSpecRep @_ @ConwayEra dvtCommitteeNoConfidence
      <*> toSpecRep @_ @ConwayEra dvtUpdateToConstitution
      <*> toSpecRep @_ @ConwayEra dvtHardForkInitiation
      <*> toSpecRep @_ @ConwayEra dvtPPNetworkGroup
      <*> toSpecRep @_ @ConwayEra dvtPPEconomicGroup
      <*> toSpecRep @_ @ConwayEra dvtPPTechnicalGroup
      <*> toSpecRep @_ @ConwayEra dvtPPGovGroup
      <*> toSpecRep @_ @ConwayEra dvtTreasuryWithdrawal

instance SpecTranslate ctx ConwayEra PoolVotingThresholds where
  type SpecRep ConwayEra PoolVotingThresholds = Agda.PoolThresholds

  toSpecRep PoolVotingThresholds {..} =
    Agda.MkPoolThresholds
      <$> toSpecRep @_ @ConwayEra pvtMotionNoConfidence
      <*> toSpecRep @_ @ConwayEra pvtCommitteeNormal
      <*> toSpecRep @_ @ConwayEra pvtCommitteeNoConfidence
      <*> toSpecRep @_ @ConwayEra pvtHardForkInitiation
      <*> toSpecRep @_ @ConwayEra pvtPPSecurityGroup

instance
  ( ConwayEraPParams era
  , PParamsHKD Identity era ~ ConwayPParams Identity era
  ) =>
  SpecTranslate ctx ConwayEra (ConwayPParams Identity era)
  where
  type SpecRep ConwayEra (ConwayPParams Identity era) = Agda.PParams

  toSpecRep cpp@ConwayPParams {..} = do
    ppA <- toSpecRep @_ @ConwayEra cppTxFeePerByte
    ppB <- toSpecRep @_ @ConwayEra cppTxFeeFixed
    ppA0 <- toSpecRep @_ @ConwayEra cppA0
    ppMinFeeRefScriptCoinsPerByte <- toSpecRep @_ @ConwayEra cppMinFeeRefScriptCostPerByte
    ppCollateralPercentage <- toSpecRep @_ @ConwayEra cppCollateralPercentage
    let
      ppMaxBlockSize = toInteger $ unTHKD cppMaxBBSize
      ppMaxTxSize = toInteger $ unTHKD cppMaxTxSize
      ppMaxHeaderSize = toInteger $ unTHKD cppMaxBHSize
    ppKeyDeposit <- toSpecRep @_ @ConwayEra cppKeyDeposit
    ppPoolDeposit <- toSpecRep @_ @ConwayEra cppPoolDeposit
    ppEmax <- toSpecRep @_ @ConwayEra cppEMax
    ppNopt <- toSpecRep @_ @ConwayEra (toInteger $ unTHKD cppNOpt)
    let
      -- We don't really care about `ppPv` in conformance testing, because
      -- the actual protocol version is stored elsewhere starting from Conway
      -- and this is just here for backwards compatibility
      ppPv = (0, 0)
      ppMinUTxOValue = 0 -- minUTxOValue has been deprecated and is not supported in Conway
    ppCoinsPerUTxOByte <- toSpecRep @_ @ConwayEra cppCoinsPerUTxOByte
    ppCostmdlsAssoc <- toSpecRep @_ @ConwayEra cppCostModels
    ppPrices <- toSpecRep @_ @ConwayEra cppPrices
    let
      pp = PParams cpp
      ppMaxRefScriptSizePerTx = toInteger $ pp ^. ppMaxRefScriptSizePerTxG
      ppMaxRefScriptSizePerBlock = toInteger $ pp ^. ppMaxRefScriptSizePerBlockG
      ppRefScriptCostStride = toInteger . unNonZero $ pp ^. ppRefScriptCostStrideG
      ppRefScriptCostMultiplier = unboundRational $ pp ^. ppRefScriptCostMultiplierG
    ppMaxTxExUnits <- toSpecRep @_ @ConwayEra cppMaxTxExUnits
    ppMaxBlockExUnits <- toSpecRep @_ @ConwayEra cppMaxBlockExUnits
    let
      ppMaxValSize = toInteger . unTHKD $ cppMaxValSize
      ppMaxCollateralInputs = toInteger . unTHKD $ cppMaxCollateralInputs
    ppPoolThresholds <- toSpecRep @_ @ConwayEra cppPoolVotingThresholds
    ppDrepThresholds <- toSpecRep @_ @ConwayEra cppDRepVotingThresholds
    let
      ppCcMinSize = toInteger . unTHKD $ cppCommitteeMinSize
      ppCcMaxTermLength = toInteger . unEpochInterval . unTHKD $ cppCommitteeMaxTermLength
    ppGovActionLifetime <- toSpecRep @_ @ConwayEra cppGovActionLifetime
    ppGovActionDeposit <- toSpecRep @_ @ConwayEra cppGovActionDeposit
    ppDrepDeposit <- toSpecRep @_ @ConwayEra cppDRepDeposit
    ppDrepActivity <- toSpecRep @_ @ConwayEra cppDRepActivity
    ppMonetaryExpansion <- toSpecRep @_ @ConwayEra cppRho
    ppTreasuryCut <- toSpecRep @_ @ConwayEra cppTau

    pure Agda.MkPParams {..}

instance SpecTranslate ctx ConwayEra ValidityInterval where
  type SpecRep ConwayEra ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep @_ @ConwayEra (lo, hi)

instance Era era => SpecTranslate ctx ConwayEra (TxDats era) where
  type SpecRep ConwayEra (TxDats era) = Agda.HSSet Agda.Datum

  toSpecRep = fmap Agda.MkHSSet . traverse (toSpecRep @_ @ConwayEra . snd) . Map.toList . unTxDats

instance SpecTranslate ctx ConwayEra (AlonzoPlutusPurpose AsIx era) where
  type SpecRep ConwayEra (AlonzoPlutusPurpose AsIx era) = Agda.RdmrPtr

  toSpecRep = \case
    AlonzoSpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    AlonzoMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    AlonzoCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    AlonzoRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)

instance SpecTranslate ctx ConwayEra (ConwayPlutusPurpose AsIx era) where
  type SpecRep ConwayEra (ConwayPlutusPurpose AsIx era) = Agda.RdmrPtr

  toSpecRep = \case
    ConwaySpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    ConwayMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    ConwayCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    ConwayRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)
    ConwayVoting (AsIx i) -> pure (Agda.Vote, toInteger i)
    ConwayProposing (AsIx i) -> pure (Agda.Propose, toInteger i)

instance
  ( AlonzoEraScript era
  , SpecTranslate ctx ConwayEra (PlutusPurpose AsIx era)
  ) =>
  SpecTranslate ctx ConwayEra (Redeemers era)
  where
  type
    SpecRep ConwayEra (Redeemers era) =
      Agda.HSMap (SpecRep ConwayEra (PlutusPurpose AsIx era)) (Agda.Redeemer, Agda.ExUnits)

  toSpecRep (Redeemers x) = toSpecRep @_ @ConwayEra x

instance
  ( AlonzoEraScript era
  , SpecTranslate ctx ConwayEra (PlutusPurpose AsIx era)
  , SpecRep ConwayEra (PlutusPurpose AsIx era) ~ Agda.RdmrPtr
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate ctx ConwayEra (AlonzoTxWits era)
  where
  type SpecRep ConwayEra (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> fmap Agda.MkHSMap (toSpecRep @_ @ConwayEra txWitsMap)
      <*> fmap Agda.MkHSSet (toSpecRep @_ @ConwayEra (Map.elems $ txscripts x))
      <*> toSpecRep @_ @ConwayEra (txdats x)
      <*> toSpecRep @_ @ConwayEra (txrdmrs x)
    where
      txWitsMap = toList (txwitsVKey x)

instance Era era => SpecTranslate ctx ConwayEra (AlonzoTxAuxData era) where
  type SpecRep ConwayEra (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep = toSpecRep @_ @ConwayEra . hashAnnotated

instance SpecTranslate ctx ConwayEra StakePoolParams where
  type SpecRep ConwayEra StakePoolParams = Agda.StakePoolParams

  toSpecRep StakePoolParams {..} =
    Agda.StakePoolParams
      <$> toSpecRep @_ @ConwayEra sppOwners
      <*> toSpecRep @_ @ConwayEra sppCost
      <*> toSpecRep @_ @ConwayEra sppMargin
      <*> toSpecRep @_ @ConwayEra sppPledge
      <*> toSpecRep @_ @ConwayEra (sppAccountAddress ^. accountAddressCredentialL)

instance SpecTranslate ctx ConwayEra DRep where
  type SpecRep ConwayEra DRep = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.VDelegCredential <$> toSpecRep @_ @ConwayEra c
  toSpecRep DRepAlwaysAbstain = pure Agda.VDelegAbstain
  toSpecRep DRepAlwaysNoConfidence = pure Agda.VDelegNoConfidence

instance SpecTranslate ctx ConwayEra Url where
  type SpecRep ConwayEra Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate ctx ConwayEra Anchor where
  type SpecRep ConwayEra Anchor = Agda.Anchor
  toSpecRep (Anchor url h) = Agda.Anchor <$> toSpecRep @_ @ConwayEra url <*> toSpecRep @_ @ConwayEra h

instance SpecTranslate ctx ConwayEra Withdrawals where
  type SpecRep ConwayEra Withdrawals = Agda.Withdrawals

  toSpecRep (Withdrawals w) = toSpecRep @_ @ConwayEra w

instance SpecTranslate ctx ConwayEra IsValid where
  type SpecRep ConwayEra IsValid = Bool

  toSpecRep (IsValid b) = pure b

instance SpecTranslate ctx ConwayEra (GovPurposeId r) where
  type SpecRep ConwayEra (GovPurposeId r) = (Agda.TxId, Integer)

  toSpecRep (GovPurposeId gaId) = toSpecRep @_ @ConwayEra gaId

instance SpecTranslate ctx ConwayEra (Committee era) where
  type SpecRep ConwayEra (Committee era) = (Agda.HSMap Agda.Credential Agda.Epoch, Agda.Rational)

  toSpecRep (Committee members threshold) = toSpecRep @_ @ConwayEra (members, threshold)

instance SpecTranslate ctx ConwayEra (Constitution era) where
  type SpecRep ConwayEra (Constitution era) = (Agda.DataHash, Maybe Agda.ScriptHash)

  toSpecRep (Constitution (Anchor _ h) policy) = toSpecRep @_ @ConwayEra (h, policy)

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx ConwayEra (EnactState era)
  where
  type SpecRep ConwayEra (EnactState era) = Agda.EnactState

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
          agdaCred <- toSpecRep @_ @ConwayEra cred
          network <- toSpecRep @_ @ConwayEra Testnet -- TODO where should this really come from?
          pure (Agda.RewardAddress network agdaCred, amount)
      transHashProtected x h = do
        committee <- toSpecRep @_ @ConwayEra x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep @_ @ConwayEra lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance SpecTranslate ctx ConwayEra Voter where
  type SpecRep ConwayEra Voter = Agda.GovVoter

  toSpecRep (CommitteeVoter c) = (Agda.CC,) <$> toSpecRep @_ @ConwayEra c
  toSpecRep (DRepVoter c) = (Agda.DRep,) <$> toSpecRep @_ @ConwayEra c
  toSpecRep (StakePoolVoter kh) = (Agda.SPO,) <$> toSpecRep @_ @ConwayEra (KeyHashObj kh)

instance SpecTranslate ctx ConwayEra Vote where
  type SpecRep ConwayEra Vote = Agda.Vote

  toSpecRep VoteYes = pure Agda.Yes
  toSpecRep VoteNo = pure Agda.No
  toSpecRep Abstain = pure Agda.Abstain

instance SpecTranslate ctx ConwayEra (VotingProcedures era) where
  type SpecRep ConwayEra (VotingProcedures era) = [Agda.GovVote]

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
                  <$> toSpecRep @_ @ConwayEra gaId
                  <*> toSpecRep @_ @ConwayEra voter
                  <*> toSpecRep @_ @ConwayEra (vProcVote votingProcedure)
                  <*> toSpecRep @_ @ConwayEra (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate ctx ConwayEra (ConwayPParams StrictMaybe era) where
  type SpecRep ConwayEra (ConwayPParams StrictMaybe era) = Agda.PParamsUpdate

  toSpecRep (ConwayPParams {..}) = do
    ppuA <- toSpecRep @_ @ConwayEra cppTxFeePerByte
    ppuB <- toSpecRep @_ @ConwayEra cppTxFeeFixed
    ppuA0 <- toSpecRep @_ @ConwayEra cppA0
    ppuMinFeeRefScriptCoinsPerByte <- toSpecRep @_ @ConwayEra cppMinFeeRefScriptCostPerByte
    ppuCollateralPercentage <- toSpecRep @_ @ConwayEra cppCollateralPercentage
    let
      ppuMaxBlockSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxBBSize
      ppuMaxTxSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxTxSize
      ppuMaxHeaderSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxBHSize
    ppuKeyDeposit <- toSpecRep @_ @ConwayEra cppKeyDeposit
    ppuPoolDeposit <- toSpecRep @_ @ConwayEra cppPoolDeposit
    ppuEmax <- toSpecRep @_ @ConwayEra cppEMax
    ppuNopt <- toSpecRep @_ @ConwayEra (fmap toInteger . strictMaybeToMaybe $ unTHKD cppNOpt)
    let
      ppuPv = Nothing
      ppuMinUTxOValue = Nothing -- minUTxOValue has been deprecated and is not supported in Conway
    ppuCoinsPerUTxOByte <- toSpecRep @_ @ConwayEra cppCoinsPerUTxOByte
    ppuCostmdls <- toSpecRep @_ @ConwayEra cppCostModels
    ppuPrices <- toSpecRep @_ @ConwayEra cppPrices
    let
      ppuMaxRefScriptSizePerTx = Nothing
      ppuMaxRefScriptSizePerBlock = Nothing
      ppuRefScriptCostStride = Nothing
      ppuRefScriptCostMultiplier = Nothing
    ppuMaxTxExUnits <- toSpecRep @_ @ConwayEra cppMaxTxExUnits
    ppuMaxBlockExUnits <- toSpecRep @_ @ConwayEra cppMaxBlockExUnits
    let
      ppuMaxValSize = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxValSize
      ppuMaxCollateralInputs = fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxCollateralInputs
    ppuPoolThresholds <- toSpecRep @_ @ConwayEra cppPoolVotingThresholds
    ppuDrepThresholds <- toSpecRep @_ @ConwayEra cppDRepVotingThresholds
    let
      ppuCcMinSize = fmap toInteger . strictMaybeToMaybe $ unTHKD cppCommitteeMinSize
      ppuCcMaxTermLength =
        fmap (toInteger . unEpochInterval) . strictMaybeToMaybe $ unTHKD cppCommitteeMaxTermLength
    ppuGovActionLifetime <- toSpecRep @_ @ConwayEra cppGovActionLifetime
    ppuGovActionDeposit <- toSpecRep @_ @ConwayEra cppGovActionDeposit
    ppuDrepDeposit <- toSpecRep @_ @ConwayEra cppDRepDeposit
    ppuDrepActivity <- toSpecRep @_ @ConwayEra cppDRepActivity
    ppuMonetaryExpansion <- toSpecRep @_ @ConwayEra cppRho
    ppuTreasuryCut <- toSpecRep @_ @ConwayEra cppTau

    pure Agda.MkPParamsUpdate {..}

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (GovAction era)
  where
  type SpecRep ConwayEra (GovAction era) = Agda.GovAction

  toSpecRep (ParameterChange _ ppu _) = Agda.ChangePParams <$> toSpecRep @_ @ConwayEra ppu
  toSpecRep (HardForkInitiation _ pv) = Agda.TriggerHardFork <$> toSpecRep @_ @ConwayEra pv
  toSpecRep (TreasuryWithdrawals withdrawals _) =
    Agda.TreasuryWithdrawal
      <$> toSpecRep @_ @ConwayEra withdrawals
  toSpecRep (NoConfidence _) = pure Agda.NoConfidence
  toSpecRep (UpdateCommittee _ remove add threshold) =
    Agda.UpdateCommittee
      <$> toSpecRep @_ @ConwayEra add
      <*> toSpecRep @_ @ConwayEra remove
      <*> toSpecRep @_ @ConwayEra threshold
  toSpecRep (NewConstitution _ (Constitution (Anchor _ h) policy)) =
    Agda.NewConstitution
      <$> toSpecRep @_ @ConwayEra h
      <*> toSpecRep @_ @ConwayEra policy
  toSpecRep InfoAction = pure Agda.Info

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (ProposalProcedure era)
  where
  type SpecRep ConwayEra (ProposalProcedure era) = Agda.GovProposal

  toSpecRep ProposalProcedure {..} =
    Agda.MkGovProposal
      <$> toSpecRep @_ @ConwayEra pProcGovAction
      <*> toSpecRep @_ @ConwayEra (nullifyIfNotNeeded prevActionId pProcGovAction)
      <*> toSpecRep @_ @ConwayEra policy
      <*> toSpecRep @_ @ConwayEra pProcDeposit
      <*> toSpecRep @_ @ConwayEra pProcReturnAddr
      <*> toSpecRep @_ @ConwayEra pProcAnchor
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
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (GovActionState era)
  where
  type SpecRep ConwayEra (GovActionState era) = Agda.GovActionState

  toSpecRep gas@GovActionState {..} = do
    Agda.MkGovActionState
      <$> ( Agda.GovVotes
              <$> toSpecRep @_ @ConwayEra gasCommitteeVotes
              <*> toSpecRep @_ @ConwayEra gasDRepVotes
              <*> toSpecRep @_ @ConwayEra gasStakePoolVotes
          )
      <*> toSpecRep @_ @ConwayEra (gasReturnAddr gas)
      <*> toSpecRep @_ @ConwayEra gasExpiresAfter
      <*> toSpecRep @_ @ConwayEra action
      <*> toSpecRep @_ @ConwayEra (nullifyIfNotNeeded (prevGovActionId action) action)
    where
      action = gasAction gas

instance SpecTranslate ctx ConwayEra GovActionIx where
  type SpecRep ConwayEra GovActionIx = Integer

  toSpecRep = pure . fromIntegral . unGovActionIx

instance SpecTranslate ctx ConwayEra GovActionId where
  type SpecRep ConwayEra GovActionId = Agda.GovActionID

  toSpecRep (GovActionId txId gaIx) = toSpecRep @_ @ConwayEra (txId, gaIx)

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (Proposals era)
  where
  type SpecRep ConwayEra (Proposals era) = Agda.GovState

  -- TODO get rid of `prioritySort` once we've changed the implementation so
  -- that the proposals are always sorted
  toSpecRep = toSpecRep @_ @ConwayEra . prioritySort . view pPropsL
    where
      prioritySort ::
        OMap GovActionId (GovActionState era) ->
        OMap GovActionId (GovActionState era)
      prioritySort = Exts.fromList . sortOn (actionPriority . gasAction) . Exts.toList

instance SpecTranslate ctx ConwayEra MaryValue where
  type SpecRep ConwayEra MaryValue = Agda.Coin

  toSpecRep = toSpecRep @_ @ConwayEra . coin

instance
  (Inject ctx Coin, ConwayEraAccounts era) =>
  SpecTranslate ctx ConwayEra (RatifyEnv era)
  where
  type SpecRep ConwayEra (RatifyEnv era) = Agda.RatifyEnv

  toSpecRep RatifyEnv {..} = do
    let
      stakeDistrs =
        Agda.StakeDistrs
          <$> toSpecRep @_ @ConwayEra reDRepDistr
          <*> toSpecRep @_ @ConwayEra reStakePoolDistr
      dreps = toSpecRep @_ @ConwayEra $ Map.map drepExpiry reDRepState
    treasury <- askCtx @Coin
    Agda.MkRatifyEnv
      <$> stakeDistrs
      <*> toSpecRep @_ @ConwayEra reCurrentEpoch
      <*> dreps
      <*> toSpecRep @_ @ConwayEra reCommitteeState
      <*> toSpecRep @_ @ConwayEra treasury
      <*> toSpecRep @_ @ConwayEra (Map.mapWithKey (stakePoolStateToStakePoolParams Testnet) reStakePools)
      <*> toSpecRep @_ @ConwayEra (Map.mapMaybe (^. dRepDelegationAccountStateL) (reAccounts ^. accountsMapL))

instance
  ( EraPParams era
  , SpecRep ConwayEra (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx ConwayEra (PParamsHKD Identity era)
  , Inject ctx [GovActionState era]
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  ) =>
  SpecTranslate ctx ConwayEra (RatifyState era)
  where
  type SpecRep ConwayEra (RatifyState era) = Agda.RatifyState

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
      <$> toSpecRep @_ @ConwayEra rsEnactState
      <*> toSpecRep @_ @ConwayEra removed
      <*> toSpecRep @_ @ConwayEra rsDelayed

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (RatifySignal era)
  where
  type
    SpecRep ConwayEra (RatifySignal era) =
      SpecRep ConwayEra [(GovActionId, GovActionState era)]

  toSpecRep (RatifySignal x) =
    toSpecRep @_ @ConwayEra $
      (\gas@GovActionState {gasId} -> (gasId, gas)) <$> x

instance
  ( EraPParams era
  , SpecTranslate ctx ConwayEra (PParamsHKD StrictMaybe era)
  , SpecRep ConwayEra (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx ConwayEra (EnactSignal era)
  where
  type SpecRep ConwayEra (EnactSignal era) = SpecRep ConwayEra (GovAction era)

  toSpecRep (EnactSignal _ ga) = toSpecRep @_ @ConwayEra ga
