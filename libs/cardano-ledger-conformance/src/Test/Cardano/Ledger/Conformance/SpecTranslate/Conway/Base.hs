{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
  SpecTranslationError,
  ConwayExecEnactEnv (..),
  DepositPurpose (..),
  ConwayTxBodyTransContext (..),
  vkeyToInteger,
  vkeyFromInteger,
  signatureToInteger,
  signatureFromInteger,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Util (bytesToNatural, naturalToBytes)
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (
  Timelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Alonzo (AlonzoTxAuxData, MaryValue)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Compactible (Compactible)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD (..))
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure,
  ConwayGovPredFailure,
  EnactSignal (..),
  maxRefScriptSizePerBlock,
  maxRefScriptSizePerTx,
 )
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..), ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.Tx (refScriptCostMultiplier, refScriptCostStride)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus (CostModels, ExUnits (..), Prices)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum (..), hashBinaryData)
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Rules (Identity)
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.State (
  CommitteeAuthorization (..),
  CommitteeState (..),
  IndividualPoolStake (..),
  PoolDistr (..),
  UTxO (..),
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (fromCompact)
import Cardano.Ledger.Val (Val (..))
import Constrained.API (HasSimpleRep, HasSpec)
import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (..))
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict (OMap)
import qualified Data.OMap.Strict as OMap
import Data.OSet.Strict (OSet)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64)
import qualified GHC.Exts as Exts
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  OpaqueErrorString (..),
  SpecTransM,
  SpecTranslate (..),
  SpecTranslationError,
  askCtx,
  hashToInteger,
  showOpaqueErrorString,
  withCtx,
 )
import Test.Cardano.Ledger.Constrained.Conway (DepositPurpose (..))
import Test.Cardano.Ledger.Constrained.Conway.Epoch
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..), showExpr)

instance SpecTranslate ctx Void where
  type SpecRep Void = Void

  toSpecRep = absurd

instance SpecTranslate ctx a => SpecTranslate ctx [a] where
  type SpecRep [a] = [SpecRep a]

  toSpecRep = traverse toSpecRep

instance SpecTranslate ctx TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate ctx TxIn where
  type SpecRep TxIn = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

instance SpecTranslate ctx StakeReference where
  type SpecRep StakeReference = Maybe Agda.Credential

  toSpecRep (StakeRefBase c) = Just <$> toSpecRep c
  toSpecRep (StakeRefPtr _) = pure Nothing
  toSpecRep StakeRefNull = pure Nothing

instance SpecTranslate ctx BootstrapAddress where
  type SpecRep BootstrapAddress = Agda.BootstrapAddr

  toSpecRep _ = error "Cannot translate bootstrap addresses"

instance SpecTranslate ctx Addr where
  type SpecRep Addr = Agda.Addr

  toSpecRep (Addr nw pc sr) =
    Left
      <$> (Agda.BaseAddr <$> toSpecRep nw <*> toSpecRep pc <*> toSpecRep sr)
  toSpecRep (AddrBootstrap ba) = Right <$> toSpecRep ba

instance SpecTranslate ctx (Hash a b) where
  type SpecRep (Hash a b) = Integer

  toSpecRep = pure . hashToInteger

instance SpecTranslate ctx (SafeHash a) where
  type SpecRep (SafeHash a) = Agda.DataHash

  toSpecRep = toSpecRep . extractHash

instance
  ( SpecRep DataHash ~ Agda.DataHash
  , Era era
  ) =>
  SpecTranslate ctx (BinaryData era)
  where
  type SpecRep (BinaryData era) = Agda.DataHash

  toSpecRep = toSpecRep . hashBinaryData

instance Era era => SpecTranslate ctx (Datum era) where
  type SpecRep (Datum era) = Maybe (Either Agda.Datum Agda.DataHash)

  toSpecRep NoDatum = pure Nothing
  toSpecRep (Datum d) = Just . Left <$> toSpecRep d
  toSpecRep (DatumHash h) = Just . Right <$> toSpecRep h

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
      <*> toSpecRep (hashScript @era $ TimelockScript tl)
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

  toSpecRep (TimelockScript s) = Left <$> toSpecRep s
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

instance SpecTranslate ctx Integer where
  type SpecRep Integer = Integer

  toSpecRep = pure

deriving instance SpecTranslate ctx Coin

instance
  ( SpecTranslate ctx (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate ctx (UTxO era)
  where
  type SpecRep (UTxO era) = SpecRep (Map TxIn (TxOut era))
  toSpecRep (UTxO m) = toSpecRep m

deriving instance SpecTranslate ctx SlotNo

deriving instance SpecTranslate ctx EpochNo

deriving instance SpecTranslate ctx EpochInterval

instance SpecTranslate ctx ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

instance SpecTranslate ctx CostModels where
  type SpecRep CostModels = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

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

instance SpecTranslate ctx (ConwayPParams Identity era) where
  type SpecRep (ConwayPParams Identity era) = Agda.PParams

  toSpecRep ConwayPParams {..} = do
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
      ppMaxRefScriptSizePerTx = toInteger maxRefScriptSizePerTx
      ppMaxRefScriptSizePerBlock = toInteger maxRefScriptSizePerBlock
      ppRefScriptCostStride = toInteger refScriptCostStride
      ppRefScriptCostMultiplier = refScriptCostMultiplier
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

instance
  SpecTranslate ctx (PParamsHKD Identity era) =>
  SpecTranslate ctx (PParams era)
  where
  type SpecRep (PParams era) = SpecRep (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance SpecTranslate ctx a => SpecTranslate ctx (Set a) where
  type SpecRep (Set a) = Agda.HSSet (SpecRep a)

  toSpecRep = fmap Agda.MkHSSet . traverse toSpecRep . Set.toList

instance SpecTranslate ctx a => SpecTranslate ctx (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate ctx a => SpecTranslate ctx (Seq a) where
  type SpecRep (Seq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate ctx a => SpecTranslate ctx (Sized a) where
  type SpecRep (Sized a) = SpecRep a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate ctx ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance SpecTranslate ctx (KeyHash r) where
  type SpecRep (KeyHash r) = Integer

  toSpecRep (KeyHash h) = toSpecRep h

vkeyToInteger :: VKey kd -> Integer
vkeyToInteger = toInteger . bytesToNatural . rawSerialiseVerKeyDSIGN . unVKey

vkeyFromInteger :: Integer -> Maybe (VKey kd)
vkeyFromInteger = fmap VKey . rawDeserialiseVerKeyDSIGN . naturalToBytes 32 . fromInteger

signatureToInteger :: DSIGNAlgorithm v => SigDSIGN v -> Integer
signatureToInteger = toInteger . bytesToNatural . rawSerialiseSigDSIGN

signatureFromInteger :: DSIGNAlgorithm v => Integer -> Maybe (SigDSIGN v)
signatureFromInteger = rawDeserialiseSigDSIGN . naturalToBytes 64 . fromInteger

instance SpecTranslate ctx (VKey k) where
  type SpecRep (VKey k) = Agda.HSVKey

  toSpecRep x = do
    let hvkVKey = vkeyToInteger x
    hvkStoredHash <- toSpecRep (hashVerKeyDSIGN @_ @ADDRHASH $ unVKey x)
    pure Agda.MkHSVKey {..}

instance DSIGNAlgorithm v => SpecTranslate ctx (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) = pure $ signatureToInteger x

instance SpecTranslate ctx (WitVKey k) where
  type SpecRep (WitVKey k) = (SpecRep (VKey k), Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep (vk, sk)

instance Era era => SpecTranslate ctx (TxDats era) where
  type SpecRep (TxDats era) = Agda.HSMap Agda.DataHash Agda.Datum

  toSpecRep (TxDats x) = toSpecRep x

instance
  ( SpecTranslate ctx k
  , SpecTranslate ctx v
  ) =>
  SpecTranslate ctx (Map k v)
  where
  type SpecRep (Map k v) = Agda.HSMap (SpecRep k) (SpecRep v)

  toSpecRep = fmap Agda.MkHSMap . traverse (bimapM toSpecRep toSpecRep) . Map.toList

instance SpecTranslate ctx Word64 where
  type SpecRep Word64 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate ctx Word32 where
  type SpecRep Word32 = Integer

  toSpecRep = pure . toInteger

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
  ( SpecTranslate ctx a
  , SpecTranslate ctx b
  ) =>
  SpecTranslate ctx (a, b)
  where
  type SpecRep (a, b) = (SpecRep a, SpecRep b)

  toSpecRep (x, y) = (,) <$> toSpecRep x <*> toSpecRep y

instance Era era => SpecTranslate ctx (Data era) where
  type SpecRep (Data era) = Agda.DataHash

  toSpecRep = toSpecRep . hashAnnotated

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

instance SpecTranslate ctx a => SpecTranslate ctx (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate ctx a => SpecTranslate ctx (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)

  toSpecRep = traverse toSpecRep

instance Era era => SpecTranslate ctx (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep = toSpecRep . hashAnnotated

instance SpecTranslate ctx ScriptHash where
  type SpecRep ScriptHash = Integer

  toSpecRep (ScriptHash h) = toSpecRep h

instance SpecTranslate ctx (Credential k) where
  type SpecRep (Credential k) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate ctx Network where
  type SpecRep Network = Integer

  toSpecRep = pure . fromIntegral . fromEnum

instance SpecTranslate ctx RewardAccount where
  type SpecRep RewardAccount = Agda.RwdAddr

  toSpecRep (RewardAccount n c) = Agda.RwdAddr <$> toSpecRep n <*> toSpecRep c

instance SpecTranslate ctx PoolParams where
  type SpecRep PoolParams = Agda.PoolParams

  toSpecRep PoolParams {..} =
    Agda.PoolParams
      <$> toSpecRep ppOwners
      <*> toSpecRep ppCost
      <*> toSpecRep ppMargin
      <*> toSpecRep ppPledge
      <*> toSpecRep (raCredential ppRewardAccount)

instance SpecTranslate ctx DRep where
  type SpecRep DRep = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.CredVoter Agda.DRep <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.AbstainRep
  toSpecRep DRepAlwaysNoConfidence = pure Agda.NoConfidenceRep

instance SpecTranslate ctx Url where
  type SpecRep Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate ctx Anchor where
  type SpecRep Anchor = Agda.Anchor
  toSpecRep (Anchor url h) = Agda.Anchor <$> toSpecRep url <*> toSpecRep h

instance SpecTranslate ctx TxId where
  type SpecRep TxId = Agda.TxId

  toSpecRep (TxId x) = toSpecRep x

instance SpecTranslate ctx Withdrawals where
  type SpecRep Withdrawals = Agda.Wdrl

  toSpecRep (Withdrawals w) = toSpecRep w

instance SpecTranslate ctx TxAuxDataHash where
  type SpecRep TxAuxDataHash = Agda.DataHash

  toSpecRep (TxAuxDataHash x) = toSpecRep x

data ConwayTxBodyTransContext = ConwayTxBodyTransContext
  { ctbtcSizeTx :: !Integer
  , ctbtcTxId :: !TxId
  }

instance Inject ConwayTxBodyTransContext Integer where
  inject = ctbtcSizeTx

instance Inject ConwayTxBodyTransContext TxId where
  inject = ctbtcTxId

instance SpecTranslate ctx IsValid where
  type SpecRep IsValid = Bool

  toSpecRep (IsValid b) = pure b

instance
  ( SpecTranslate ctx (TxWits era)
  , SpecTranslate ctx (TxAuxData era)
  , SpecTranslate ConwayTxBodyTransContext (TxBody era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxBody era) ~ Agda.TxBody
  , Tx era ~ AlonzoTx era
  , EraTx era
  , BabbageEraTxBody era
  , AlonzoEraTx era
  ) =>
  SpecTranslate ctx (AlonzoTx era)
  where
  type SpecRep (AlonzoTx era) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtx
        (ConwayTxBodyTransContext (tx ^. sizeTxF) (txIdTx tx))
        (toSpecRep (tx ^. bodyTxL))
      <*> toSpecRep (tx ^. witsTxL)
      <*> toSpecRep (tx ^. isValidTxL)
      <*> toSpecRep (tx ^. auxDataTxL)

instance
  ( EraPParams era
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  SpecTranslate ctx (ConwayGovPredFailure era)
  where
  type SpecRep (ConwayGovPredFailure era) = OpaqueErrorString

  toSpecRep = pure . showOpaqueErrorString

instance SpecTranslate ctx (GovPurposeId r) where
  type SpecRep (GovPurposeId r) = (Agda.TxId, Integer)

  toSpecRep (GovPurposeId gaId) = toSpecRep gaId

instance SpecTranslate ctx UnitInterval where
  type SpecRep UnitInterval = Agda.Rational

  toSpecRep = pure . unboundRational

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
  type SpecRep Voter = Agda.Voter

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
          <$> ( Agda.GovVote
                  <$> toSpecRep gaId
                  <*> toSpecRep voter
                  <*> toSpecRep (vProcVote votingProcedure)
                  <*> toSpecRep (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate ctx Word16 where
  type SpecRep Word16 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate ctx NonNegativeInterval where
  type SpecRep NonNegativeInterval = Agda.Rational

  toSpecRep = pure . unboundRational

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
  SpecTranslate ctx (PParamsHKD StrictMaybe era) =>
  SpecTranslate ctx (PParamsUpdate era)
  where
  type SpecRep (PParamsUpdate era) = SpecRep (PParamsHKD StrictMaybe era)

  toSpecRep (PParamsUpdate ppu) = toSpecRep ppu

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (GovAction era)
  where
  type SpecRep (GovAction era) = Agda.GovAction

  toSpecRep (ParameterChange _ ppu _) = Agda.ChangePParams <$> toSpecRep ppu
  toSpecRep (HardForkInitiation _ pv) = Agda.TriggerHF <$> toSpecRep pv
  toSpecRep (TreasuryWithdrawals withdrawals _) =
    Agda.TreasuryWdrl
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

instance SpecTranslate ctx a => SpecTranslate ctx (OSet a) where
  type SpecRep (OSet a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance
  ( SpecTranslate ctx k
  , SpecTranslate ctx v
  , Ord k
  ) =>
  SpecTranslate ctx (OMap k v)
  where
  type SpecRep (OMap k v) = [(SpecRep k, SpecRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . OMap.assocList

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

unionsHSMaps :: [Agda.HSMap k v] -> Agda.HSMap k v
unionsHSMaps [] = Agda.MkHSMap []
unionsHSMaps ((Agda.MkHSMap x) : xs) =
  let Agda.MkHSMap xs' = unionsHSMaps xs
   in Agda.MkHSMap $ x <> xs'

mapHSMapKey :: (k -> l) -> Agda.HSMap k v -> Agda.HSMap l v
mapHSMapKey f (Agda.MkHSMap l) = Agda.MkHSMap $ first f <$> l

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
      <$> agdaVoteMap
      <*> toSpecRep (gasReturnAddr gas)
      <*> toSpecRep gasExpiresAfter
      <*> toSpecRep action
      <*> toSpecRep (nullifyIfNotNeeded (prevGovActionId action) action)
    where
      action = gasAction gas
      agdaVoteMap :: SpecTransM ctx (Agda.HSMap (Agda.GovRole, Agda.Credential) Agda.Vote)
      agdaVoteMap = do
        drepVotes <- toSpecRep gasDRepVotes
        ccVotes <- toSpecRep gasCommitteeVotes
        spoVotes <- toSpecRep gasStakePoolVotes
        pure $
          unionsHSMaps
            [ mapHSMapKey (Agda.DRep,) drepVotes
            , mapHSMapKey (Agda.CC,) ccVotes
            , mapHSMapKey (\h -> (Agda.SPO, Agda.KeyHashObj h)) spoVotes
            ]

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
  ( ToExpr (PredicateFailure (EraRule "DELEG" era))
  , ToExpr (PredicateFailure (EraRule "GOVCERT" era))
  , ToExpr (PredicateFailure (EraRule "POOL" era))
  ) =>
  SpecTranslate ctx (ConwayCertPredFailure era)
  where
  type SpecRep (ConwayCertPredFailure era) = OpaqueErrorString

  toSpecRep = pure . showOpaqueErrorString

instance (SpecTranslate ctx a, Compactible a) => SpecTranslate ctx (CompactForm a) where
  type SpecRep (CompactForm a) = SpecRep a

  toSpecRep = toSpecRep . fromCompact

instance SpecTranslate ctx CommitteeAuthorization where
  type
    SpecRep CommitteeAuthorization =
      SpecRep (Maybe (Credential 'HotCommitteeRole))

  toSpecRep (CommitteeHotCredential c) = toSpecRep $ Just c
  toSpecRep (CommitteeMemberResigned _) =
    toSpecRep $
      Nothing @(Credential 'HotCommitteeRole)

instance SpecTranslate ctx (CommitteeState era) where
  type
    SpecRep (CommitteeState era) =
      SpecRep (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)

  toSpecRep = toSpecRep . csCommitteeCreds

instance SpecTranslate ctx IndividualPoolStake where
  type SpecRep IndividualPoolStake = SpecRep Coin

  toSpecRep (IndividualPoolStake _ c _) = toSpecRep c

instance SpecTranslate ctx PoolDistr where
  type SpecRep PoolDistr = Agda.HSMap Agda.VDeleg Agda.Coin

  toSpecRep (PoolDistr ps _) = do
    Agda.MkHSMap l <- toSpecRep ps
    pure . Agda.MkHSMap $ first (Agda.CredVoter Agda.SPO . Agda.KeyHashObj) <$> l

instance
  Inject ctx Coin =>
  SpecTranslate ctx (RatifyEnv era)
  where
  type SpecRep (RatifyEnv era) = Agda.RatifyEnv

  toSpecRep RatifyEnv {..} = do
    let
      stakeDistrs = do
        Agda.MkHSMap stakeDistrsMap <- toSpecRep reStakePoolDistr
        drepDistrsMap <- toSpecRep $ Map.toList reDRepDistr
        pure . Agda.StakeDistrs $ Agda.MkHSMap (stakeDistrsMap <> drepDistrsMap)
      dreps = toSpecRep $ Map.map drepExpiry reDRepState
    treasury <- askCtx @Coin
    Agda.MkRatifyEnv
      <$> stakeDistrs
      <*> toSpecRep reCurrentEpoch
      <*> dreps
      <*> toSpecRep reCommitteeState
      <*> toSpecRep treasury
      <*> toSpecRep rePoolParams
      <*> toSpecRep reDelegatees

instance SpecTranslate ctx Bool where
  type SpecRep Bool = Bool

  toSpecRep = pure

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

instance ToExpr (EpochExecEnv era)

instance Era era => NFData (EpochExecEnv era)

instance HasSimpleRep (EpochExecEnv era)

instance Era era => HasSpec (EpochExecEnv era)

instance SpecTranslate ctx (EpochExecEnv era) where
  type SpecRep (EpochExecEnv era) = ()

  toSpecRep _ = pure ()

-- | This type is used as the Env only in the Agda Spec
data ConwayExecEnactEnv era = ConwayExecEnactEnv
  { ceeeGid :: GovActionId
  , ceeeTreasury :: Coin
  , ceeeEpoch :: EpochNo
  }
  deriving (Generic, Eq, Show)

-- | Here we inject the Agda Spec Env into the STS rule Environment, which is ().
instance Inject (ConwayExecEnactEnv era) () where
  inject _ = ()

instance ToExpr (ConwayExecEnactEnv era)

instance Era era => NFData (ConwayExecEnactEnv era)

instance HasSimpleRep (ConwayExecEnactEnv era)

instance Era era => HasSpec (ConwayExecEnactEnv era)

instance SpecTranslate ctx (ConwayExecEnactEnv era) where
  type SpecRep (ConwayExecEnactEnv era) = Agda.EnactEnv

  toSpecRep ConwayExecEnactEnv {..} =
    Agda.MkEnactEnv
      <$> toSpecRep ceeeGid
      <*> toSpecRep ceeeTreasury
      <*> toSpecRep ceeeEpoch

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization ->
  StrictMaybe (Credential 'HotCommitteeRole)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance SpecTranslate ctx DepositPurpose where
  type SpecRep DepositPurpose = Agda.DepositPurpose
  toSpecRep (CredentialDeposit cred) =
    Agda.CredentialDeposit <$> toSpecRep cred
  toSpecRep (PoolDeposit kh) =
    Agda.PoolDeposit <$> toSpecRep kh
  toSpecRep (DRepDeposit cred) =
    Agda.DRepDeposit <$> toSpecRep cred
  toSpecRep (GovActionDeposit gid) =
    Agda.GovActionDeposit <$> toSpecRep gid
