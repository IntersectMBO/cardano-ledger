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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
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
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core (committeeCredentialToStrictMaybe)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..), showExpr)

instance SpecTranslate TxId where
  type SpecRep TxId = Agda.TxId

  toSpecRep (TxId x) = toSpecRep x

instance SpecTranslate TxIn where
  type SpecRep TxIn = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRepTuple (txId, txIx)

instance SpecTranslate (SafeHash a) where
  type SpecRep (SafeHash a) = Agda.DataHash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate Language where
  type SpecRep Language = Agda.HSLanguage

  toSpecRep l = case l of
    PlutusV1 -> return Agda.PV1
    PlutusV2 -> return Agda.PV2
    PlutusV3 -> return Agda.PV3
    PlutusV4 -> error "PlutusV4 not supported"

instance SpecTranslate CostModels where
  type SpecRep CostModels = Agda.LanguageCostModels

  toSpecRep cm =
    -- filter out PlutusV4 language
    let validCostModels = filter ((/= PlutusV4) . fst) $ Map.toList (costModelsValid cm)
     in Agda.MkLanguageCostModels <$> mapM (\(l, _) -> (,()) <$> toSpecRep l) validCostModels

instance SpecTranslate ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

instance
  ( SpecRep DataHash ~ Agda.DataHash
  , Era era
  ) =>
  SpecTranslate (BinaryData era)
  where
  type SpecRep (BinaryData era) = Agda.DataHash

  toSpecRep = toSpecRep . hashBinaryData

instance Era era => SpecTranslate (Datum era) where
  type SpecRep (Datum era) = Maybe (Either Agda.Datum Agda.DataHash)

  toSpecRep NoDatum = pure Nothing
  toSpecRep (Datum d) = Just . Left <$> toSpecRep d
  toSpecRep (DatumHash h) = Just . Right <$> toSpecRep h

instance Era era => SpecTranslate (Data era) where
  type SpecRep (Data era) = Agda.DataHash

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate TxAuxDataHash where
  type SpecRep TxAuxDataHash = Agda.DataHash

  toSpecRep (TxAuxDataHash x) = toSpecRep x

instance
  ( AlonzoEraScript era
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate (Timelock era)
  where
  type SpecRep (Timelock era) = Agda.HSTimelock

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

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate (PlutusScript era)
  where
  type SpecRep (PlutusScript era) = Agda.HSPlutusScript

  toSpecRep ps =
    Agda.MkHSPlutusScript
      <$> toSpecRep (hashScript $ PlutusScript ps)
      <*> pure (fromIntegral $ originalBytesSize ps)
      <*> toSpecRep (plutusScriptLanguage ps)

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate (AlonzoScript era)
  where
  type SpecRep (AlonzoScript era) = Agda.Script

  toSpecRep (NativeScript s) = Left <$> toSpecRep s
  toSpecRep (PlutusScript s) = Right <$> toSpecRep s

instance
  ( EraTxOut era
  , SpecRep (Value era) ~ Agda.Coin
  , SpecContext (Value era) ~ ()
  , Script era ~ AlonzoScript era
  , SpecTranslate (Value era)
  , SpecTranslate (Script era)
  ) =>
  SpecTranslate (BabbageTxOut era)
  where
  type SpecRep (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum script) = do
    addr' <- toSpecRep addr
    val' <- toSpecRep val
    datum' <- toSpecRep datum
    script' <- toSpecRep script
    pure (addr', (val', (datum', script')))

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecContext (TxOut era) ~ ()
  ) =>
  SpecTranslate (UTxO era)
  where
  type SpecRep (UTxO era) = Agda.HSMap (SpecRep TxIn) (SpecRep (TxOut era))

  toSpecRep (UTxO m) = toSpecRepMap m

deriving instance SpecTranslate OrdExUnits

deriving instance SpecTranslate CoinPerByte

instance
  SpecTranslate (HKD f a) =>
  SpecTranslate (THKD r f a)
  where
  type SpecRep (THKD r f a) = SpecRep (HKD f a)
  type SpecContext (THKD r f a) = SpecContext (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate DRepVotingThresholds where
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

instance SpecTranslate PoolVotingThresholds where
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
  , PParamsHKD Shelley.Identity era ~ ConwayPParams Shelley.Identity era
  ) =>
  SpecTranslate (ConwayPParams Shelley.Identity era)
  where
  type SpecRep (ConwayPParams Shelley.Identity era) = Agda.PParams

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

instance SpecTranslate ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRepTuple (lo, hi)

instance Era era => SpecTranslate (TxDats era) where
  type SpecRep (TxDats era) = Agda.HSSet Agda.Datum

  toSpecRep = fmap Agda.MkHSSet . traverse (toSpecRep . snd) . Map.toList . unTxDats

instance SpecTranslate (AlonzoPlutusPurpose AsIx era) where
  type SpecRep (AlonzoPlutusPurpose AsIx era) = Agda.RdmrPtr

  toSpecRep = \case
    AlonzoSpending (AsIx i) -> pure (Agda.Spend, toInteger i)
    AlonzoMinting (AsIx i) -> pure (Agda.Mint, toInteger i)
    AlonzoCertifying (AsIx i) -> pure (Agda.Cert, toInteger i)
    AlonzoRewarding (AsIx i) -> pure (Agda.Rewrd, toInteger i)

instance SpecTranslate (ConwayPlutusPurpose AsIx era) where
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
  , SpecTranslate (PlutusPurpose AsIx era)
  , SpecContext (PlutusPurpose AsIx era) ~ ()
  , SpecRep (Data era) ~ Agda.Redeemer
  ) =>
  SpecTranslate (Redeemers era)
  where
  type
    SpecRep (Redeemers era) =
      Agda.HSMap (SpecRep (PlutusPurpose AsIx era)) (Agda.Redeemer, Agda.ExUnits)

  toSpecRep (Redeemers x) =
    fmap Agda.MkHSMap
      . traverse (toSpecRepTupleGen toSpecRep toSpecRepTuple)
      . Map.toList
      $ x

instance
  ( AlonzoEraScript era
  , SpecTranslate (PlutusPurpose AsIx era)
  , SpecRep (PlutusPurpose AsIx era) ~ Agda.RdmrPtr
  , SpecContext (PlutusPurpose AsIx era) ~ ()
  , SpecRep (Data era) ~ Agda.Redeemer
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate (AlonzoTxWits era)
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

instance Era era => SpecTranslate (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate StakePoolParams where
  type SpecRep StakePoolParams = Agda.StakePoolParams

  toSpecRep StakePoolParams {..} =
    Agda.StakePoolParams
      <$> toSpecRep sppOwners
      <*> toSpecRep sppCost
      <*> toSpecRep sppMargin
      <*> toSpecRep sppPledge
      <*> toSpecRep (sppAccountAddress ^. accountAddressCredentialL)

instance SpecTranslate DRep where
  type SpecRep DRep = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.VDelegCredential <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.VDelegAbstain
  toSpecRep DRepAlwaysNoConfidence = pure Agda.VDelegNoConfidence

instance SpecTranslate Url where
  type SpecRep Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate Anchor where
  type SpecRep Anchor = Agda.Anchor
  toSpecRep (Anchor url h) = Agda.Anchor <$> toSpecRep url <*> toSpecRep h

instance SpecTranslate Withdrawals where
  type SpecRep Withdrawals = Agda.Withdrawals

  toSpecRep (Withdrawals w) = toSpecRepMap w

instance SpecTranslate IsValid where
  type SpecRep IsValid = Bool

  toSpecRep (IsValid b) = pure b

instance SpecTranslate (GovPurposeId r) where
  type SpecRep (GovPurposeId r) = (Agda.TxId, Integer)

  toSpecRep (GovPurposeId gaId) = toSpecRep gaId

instance SpecTranslate (Committee era) where
  type SpecRep (Committee era) = (Agda.HSMap Agda.Credential Agda.Epoch, Agda.Rational)

  toSpecRep (Committee members threshold) = (,) <$> toSpecRepMap members <*> toSpecRep threshold

instance SpecTranslate (Constitution era) where
  type SpecRep (Constitution era) = (Agda.DataHash, Maybe Agda.ScriptHash)

  toSpecRep (Constitution (Anchor _ h) policy) = toSpecRepTuple (h, policy)

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD Shelley.Identity era)
  , SpecRep (PParamsHKD Shelley.Identity era) ~ Agda.PParams
  , SpecContext (PParamsHKD Shelley.Identity era) ~ ()
  ) =>
  SpecTranslate (EnactState era)
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
          pure (Agda.RewardAddress network agdaCred, amount)
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance SpecTranslate Voter where
  type SpecRep Voter = Agda.GovVoter

  toSpecRep (CommitteeVoter c) = (Agda.CC,) <$> toSpecRep c
  toSpecRep (DRepVoter c) = (Agda.DRep,) <$> toSpecRep c
  toSpecRep (StakePoolVoter kh) = (Agda.SPO,) <$> toSpecRep (KeyHashObj kh)

instance SpecTranslate Vote where
  type SpecRep Vote = Agda.Vote

  toSpecRep VoteYes = pure Agda.Yes
  toSpecRep VoteNo = pure Agda.No
  toSpecRep Abstain = pure Agda.Abstain

instance SpecTranslate (VotingProcedures era) where
  type SpecRep (VotingProcedures era) = [Agda.GovVote]

  toSpecRep = foldrVotingProcedures go (pure [])
    where
      go ::
        Voter ->
        GovActionId ->
        VotingProcedure era ->
        SpecTransM () [Agda.GovVote] ->
        SpecTransM () [Agda.GovVote]
      go voter gaId votingProcedure m =
        (:)
          <$> ( Agda.MkGovVote
                  <$> toSpecRep gaId
                  <*> toSpecRep voter
                  <*> toSpecRep (vProcVote votingProcedure)
                  <*> toSpecRep (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate (ConwayPParams StrictMaybe era) where
  type SpecRep (ConwayPParams StrictMaybe era) = Agda.PParamsUpdate

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

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  ) =>
  SpecTranslate (GovAction era)
  where
  type SpecRep (GovAction era) = Agda.GovAction

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

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  ) =>
  SpecTranslate (ProposalProcedure era)
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
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  ) =>
  SpecTranslate (GovActionState era)
  where
  type SpecRep (GovActionState era) = Agda.GovActionState

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

instance SpecTranslate GovActionIx where
  type SpecRep GovActionIx = Integer

  toSpecRep = pure . fromIntegral . unGovActionIx

instance SpecTranslate GovActionId where
  type SpecRep GovActionId = Agda.GovActionID

  toSpecRep (GovActionId txId gaIx) = toSpecRepTuple (txId, gaIx)

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  ) =>
  SpecTranslate (Proposals era)
  where
  type SpecRep (Proposals era) = Agda.GovState

  -- TODO get rid of `prioritySort` once we've changed the implementation so
  -- that the proposals are always sorted
  toSpecRep = toSpecRepOMap . prioritySort . view pPropsL
    where
      prioritySort ::
        OMap GovActionId (GovActionState era) ->
        OMap GovActionId (GovActionState era)
      prioritySort = Exts.fromList . sortOn (actionPriority . gasAction) . Exts.toList

instance SpecTranslate MaryValue where
  type SpecRep MaryValue = Agda.Coin

  toSpecRep = toSpecRep . coin

instance
  ConwayEraAccounts era =>
  SpecTranslate (RatifyEnv era)
  where
  type SpecRep (RatifyEnv era) = Agda.RatifyEnv
  type SpecContext (RatifyEnv era) = Coin

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
        <*> toSpecRepMap (Map.mapMaybe (^. dRepDelegationAccountStateL) (reAccounts ^. accountsMapL))

instance
  ( EraPParams era
  , SpecRep (PParamsHKD Shelley.Identity era) ~ Agda.PParams
  , SpecContext (PParamsHKD Shelley.Identity era) ~ ()
  , SpecTranslate (PParamsHKD Shelley.Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  , SpecTranslate (PParamsHKD StrictMaybe era)
  ) =>
  SpecTranslate (RatifyState era)
  where
  type SpecRep (RatifyState era) = Agda.RatifyState
  type SpecContext (RatifyState era) = [GovActionState era]

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

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  ) =>
  SpecTranslate (RatifySignal era)
  where
  type
    SpecRep (RatifySignal era) =
      [(SpecRep GovActionId, SpecRep (GovActionState era))]

  toSpecRep (RatifySignal x) =
    traverse (\gas@GovActionState {gasId} -> toSpecRepTuple (gasId, gas)) (toList x)

instance
  ( EraPParams era
  , SpecTranslate (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecContext (PParamsHKD StrictMaybe era) ~ ()
  ) =>
  SpecTranslate (Conway.EnactSignal era)
  where
  type SpecRep (Conway.EnactSignal era) = SpecRep (GovAction era)

  toSpecRep (Conway.EnactSignal _ ga) = toSpecRep ga
