{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Util (bytesToNatural)
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
import Cardano.Ledger.Alonzo.TxAuxData (AuxiliaryDataHash)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecShareCBOR (..), Interns, Sized (..))
import Cardano.Ledger.CertState (CommitteeAuthorization (..), CommitteeState (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Compactible (Compactible)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD (..))
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure,
  ConwayGovPredFailure,
  ConwayUtxoPredFailure,
  EnactSignal (..),
  maxRefScriptSizePerBlock,
  maxRefScriptSizePerTx,
 )
import Cardano.Ledger.Conway.Scripts (AlonzoScript (..), ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.Tx (refScriptCostMultiplier, refScriptCostStride)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus (CostModels, ExUnits (..), Prices)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum (..), hashBinaryData)
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (Identity, UtxoEnv (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (fromCompact)
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Constrained (HasSimpleRep, HasSpec)
import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (..))
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.Data (Typeable)
import Data.Default.Class (Default (..))
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
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  OpaqueErrorString (..),
  SpecTransM,
  SpecTranslate (..),
  SpecTranslationError,
  askCtx,
  hashToInteger,
  withCtx,
 )
import Test.Cardano.Ledger.Constrained.Conway (DepositPurpose (..), IsConwayUniv)
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

instance SpecTranslate ctx (TxIn era) where
  type SpecRep (TxIn era) = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

instance SpecTranslate ctx (StakeReference era) where
  type SpecRep (StakeReference era) = Agda.Credential

  toSpecRep (StakeRefBase c) = toSpecRep c
  toSpecRep (StakeRefPtr _) = error "Cannot translate StakeRefPtr"
  toSpecRep StakeRefNull = error "Cannot translate StakeRefNull"

instance SpecTranslate ctx (BootstrapAddress era) where
  type SpecRep (BootstrapAddress era) = Agda.BootstrapAddr

  toSpecRep _ = error "Cannot translate bootstrap addresses"

instance SpecTranslate ctx (Addr era) where
  type SpecRep (Addr era) = Agda.Addr

  toSpecRep (Addr nw pc sr) =
    Left
      <$> (Agda.BaseAddr <$> toSpecRep nw <*> toSpecRep pc <*> toSpecRep sr)
  toSpecRep (AddrBootstrap ba) = Right <$> toSpecRep ba

instance SpecTranslate ctx (Hash a b) where
  type SpecRep (Hash a b) = Integer

  toSpecRep = pure . hashToInteger

instance SpecTranslate ctx (SafeHash c EraIndependentData) where
  type SpecRep (SafeHash c EraIndependentData) = Agda.DataHash

  toSpecRep _ = pure ()

instance SpecTranslate ctx (SafeHash c EraIndependentTxBody) where
  type SpecRep (SafeHash c EraIndependentTxBody) = Integer

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate ctx (SafeHash c EraIndependentScriptIntegrity) where
  type SpecRep (SafeHash c EraIndependentScriptIntegrity) = Integer

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate ctx (SafeHash c AnchorData) where
  type SpecRep (SafeHash c AnchorData) = Agda.DataHash

  toSpecRep _ = pure ()

instance
  ( SpecRep (DataHash (EraCrypto era)) ~ Agda.DataHash
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
  type SpecRep (Timelock era) = Agda.HashedTimelock

  toSpecRep tl =
    Agda.HashedTimelock
      <$> timelockToSpecRep tl
      <*> toSpecRep (hashScript @era $ TimelockScript tl)
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

instance
  ( AlonzoEraScript era
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  ) =>
  SpecTranslate ctx (PlutusScript era)
  where
  type SpecRep (PlutusScript era) = Agda.ScriptHash

  toSpecRep ps = toSpecRep . hashScript $ PlutusScript @era ps

instance
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SpecTranslate ctx (AlonzoScript era)
  where
  type SpecRep (AlonzoScript era) = Agda.Script

  toSpecRep (TimelockScript s) = Left <$> toSpecRep s
  toSpecRep (PlutusScript _) = pure $ Right ()

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
  type SpecRep (UTxO era) = SpecRep (Map (TxIn (EraCrypto era)) (TxOut era))
  toSpecRep (UTxO m) = toSpecRep m

instance
  ( SpecTranslate ctx (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , GovState era ~ ConwayGovState era
  ) =>
  SpecTranslate ctx (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState

  toSpecRep UTxOState {..} = do
    let
      gasDeposits =
        Map.fromList . fmap (bimap GovActionDeposit gasDeposit) $
          OMap.assocList (utxosGovState ^. cgsProposalsL . pPropsL)
    Agda.MkUTxOState
      <$> toSpecRep utxosUtxo
      <*> toSpecRep utxosFees
      <*> toSpecRep gasDeposits
      <*> toSpecRep utxosDonation

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

    pure Agda.MkPParams {..}

instance
  SpecTranslate ctx (PParamsHKD Identity era) =>
  SpecTranslate ctx (PParams era)
  where
  type SpecRep (PParams era) = SpecRep (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance
  ( SpecRep (PParams era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (UtxoEnv era)
  where
  type SpecRep (UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (ueSlot x)
      <*> toSpecRep (uePParams x)
      <*> toSpecRep (Coin 10_000_000) -- TODO: Fix generating types

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

instance SpecTranslate ctx (KeyHash r c) where
  type SpecRep (KeyHash r c) = Integer

  toSpecRep (KeyHash h) = toSpecRep h

instance Crypto c => SpecTranslate ctx (VKey k c) where
  type SpecRep (VKey k c) = Integer

  toSpecRep (VKey x) = pure . toInteger . bytesToNatural $ rawSerialiseVerKeyDSIGN x

instance DSIGNAlgorithm v => SpecTranslate ctx (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) =
    pure . toInteger . bytesToNatural $ rawSerialiseSigDSIGN x

instance (Crypto c, Typeable k) => SpecTranslate ctx (WitVKey k c) where
  type SpecRep (WitVKey k c) = (Integer, Integer)

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

instance SpecTranslate ctx (Data era) where
  type SpecRep (Data era) = ()

  toSpecRep _ = pure ()

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

instance SpecTranslate ctx (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep _ = pure ()

instance SpecTranslate ctx (ScriptHash c) where
  type SpecRep (ScriptHash c) = Integer

  toSpecRep (ScriptHash h) = toSpecRep h

instance SpecTranslate ctx (Credential k c) where
  type SpecRep (Credential k c) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate ctx Network where
  type SpecRep Network = ()

  toSpecRep = pure . const ()

instance SpecTranslate ctx (RewardAccount c) where
  type SpecRep (RewardAccount c) = Agda.RwdAddr

  toSpecRep (RewardAccount n c) = Agda.RwdAddr <$> toSpecRep n <*> toSpecRep c

instance SpecTranslate ctx (PoolParams era) where
  type SpecRep (PoolParams era) = Agda.PoolParams

  toSpecRep PoolParams {..} = Agda.PoolParams <$> toSpecRep (KeyHashObj ppId)

instance SpecTranslate ctx (DRep c) where
  type SpecRep (DRep c) = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.CredVoter Agda.DRep <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.AbstainRep
  toSpecRep DRepAlwaysNoConfidence = pure Agda.NoConfidenceRep

instance SpecTranslate ctx Url where
  type SpecRep Url = T.Text
  toSpecRep = pure . urlToText

instance SpecTranslate ctx (Anchor c) where
  type SpecRep (Anchor c) = Agda.Anchor
  toSpecRep (Anchor url _) = Agda.Anchor <$> toSpecRep url <*> pure ()

instance SpecTranslate ctx (TxId era) where
  type SpecRep (TxId era) = Agda.TxId

  toSpecRep (TxId x) = toSpecRep x

instance SpecTranslate ctx (Withdrawals era) where
  type SpecRep (Withdrawals era) = Agda.Wdrl

  toSpecRep (Withdrawals w) = toSpecRep w

instance SpecTranslate ctx (AuxiliaryDataHash c) where
  type SpecRep (AuxiliaryDataHash c) = ()

  toSpecRep _ = pure ()

instance
  ( ConwayEraTxBody era
  , TxBody era ~ ConwayTxBody era
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (ConwayTxCert era) ~ Agda.DCert
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , TxCert era ~ ConwayTxCert era
  , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
  , Inject ctx Integer
  , Inject ctx (TxId (EraCrypto era))
  , SpecTranslate ctx (TxOut era)
  , SpecTranslate ctx (ConwayTxCert era)
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  ) =>
  SpecTranslate ctx (ConwayTxBody era)
  where
  type SpecRep (ConwayTxBody era) = Agda.TxBody

  toSpecRep txb = do
    sizeTx <- askCtx
    txId <- askCtx @(TxId (EraCrypto era))
    Agda.MkTxBody
      <$> toSpecRep (txb ^. inputsTxBodyL)
      <*> toSpecRep (txb ^. referenceInputsTxBodyL)
      <*> (Agda.MkHSMap . zip [0 ..] <$> toSpecRep (txb ^. outputsTxBodyL))
      <*> toSpecRep (txb ^. feeTxBodyL)
      <*> pure 0
      <*> toSpecRep (txb ^. vldtTxBodyL)
      <*> toSpecRep (txb ^. certsTxBodyL)
      <*> toSpecRep (txb ^. withdrawalsTxBodyL)
      <*> toSpecRep (txb ^. votingProceduresTxBodyL)
      <*> toSpecRep (txb ^. proposalProceduresTxBodyL)
      <*> toSpecRep (txb ^. treasuryDonationTxBodyL)
      <*> pure Nothing -- TODO implement this properly
      <*> toSpecRep (txb ^. auxDataHashTxBodyL)
      <*> toSpecRep (txb ^. networkIdTxBodyL)
      <*> toSpecRep (txb ^. currentTreasuryValueTxBodyL)
      <*> pure sizeTx
      <*> toSpecRep txId
      <*> toSpecRep (txb ^. collateralInputsTxBodyL)
      <*> toSpecRep (txb ^. reqSignerHashesTxBodyL)
      <*> toSpecRep (txb ^. scriptIntegrityHashTxBodyL)

data ConwayTxBodyTransContext c = ConwayTxBodyTransContext
  { ctbtcSizeTx :: !Integer
  , ctbtcTxId :: !(TxId c)
  }

instance Inject (ConwayTxBodyTransContext c) Integer where
  inject = ctbtcSizeTx

instance Inject (ConwayTxBodyTransContext c) (TxId c) where
  inject = ctbtcTxId

instance SpecTranslate ctx IsValid where
  type SpecRep IsValid = Bool

  toSpecRep (IsValid b) = pure b

instance
  ( SpecTranslate ctx (TxWits era)
  , SpecTranslate ctx (TxAuxData era)
  , SpecTranslate (ConwayTxBodyTransContext (EraCrypto era)) (TxBody era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxBody era) ~ Agda.TxBody
  , Tx era ~ AlonzoTx era
  , EraTx era
  , BabbageEraTxBody era
  ) =>
  SpecTranslate ctx (AlonzoTx era)
  where
  type SpecRep (AlonzoTx era) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> withCtx
        (ConwayTxBodyTransContext @(EraCrypto era) (tx ^. sizeTxF) (txIdTx tx))
        (toSpecRep (body tx))
      <*> toSpecRep (wits tx)
      <*> toSpecRep (isValid tx)
      <*> toSpecRep (auxiliaryData tx)

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  SpecTranslate ctx (ConwayUtxoPredFailure era)
  where
  type SpecRep (ConwayUtxoPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e

instance
  ( EraPParams era
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  SpecTranslate ctx (ConwayGovPredFailure era)
  where
  type SpecRep (ConwayGovPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e

instance SpecTranslate ctx (GovPurposeId r c) where
  type SpecRep (GovPurposeId r c) = (Agda.TxId, Integer)

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
          pure (Agda.RwdAddr () agdaCred, amount)
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance SpecTranslate ctx (Voter era) where
  type SpecRep (Voter era) = Agda.Voter

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
        Voter (EraCrypto era) ->
        GovActionId (EraCrypto era) ->
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
    ppuNopt <- toSpecRep (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppNOpt)
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

prevGovActionId :: GovAction era -> StrictMaybe (GovActionId (EraCrypto era))
prevGovActionId action =
  case action of
    ParameterChange x _ _ -> unGovPurposeId <$> x
    HardForkInitiation x _ -> unGovPurposeId <$> x
    UpdateCommittee x _ _ _ -> unGovPurposeId <$> x
    NoConfidence x -> unGovPurposeId <$> x
    NewConstitution x _ -> unGovPurposeId <$> x
    _ -> SNothing

nullGovActionId :: Crypto c => GovActionId c
nullGovActionId = GovActionId (TxId def) (GovActionIx 0)

nullifyIfNotNeeded :: Crypto c => StrictMaybe (GovActionId c) -> GovAction era -> GovActionId c
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

instance SpecTranslate ctx (GovActionId c) where
  type SpecRep (GovActionId c) = Agda.GovActionID

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
        OMap (GovActionId (EraCrypto era)) (GovActionState era) ->
        OMap (GovActionId (EraCrypto era)) (GovActionState era)
      prioritySort = Exts.fromList . sortOn (actionPriority . gasAction) . Exts.toList

instance Crypto c => SpecTranslate ctx (MaryValue c) where
  type SpecRep (MaryValue c) = Agda.Coin

  toSpecRep = toSpecRep . coin

instance
  ( ToExpr (PredicateFailure (EraRule "DELEG" era))
  , ToExpr (PredicateFailure (EraRule "GOVCERT" era))
  , ToExpr (PredicateFailure (EraRule "POOL" era))
  ) =>
  SpecTranslate ctx (ConwayCertPredFailure era)
  where
  type SpecRep (ConwayCertPredFailure era) = OpaqueErrorString

  toSpecRep = pure . OpaqueErrorString . showExpr

instance (SpecTranslate ctx a, Compactible a) => SpecTranslate ctx (CompactForm a) where
  type SpecRep (CompactForm a) = SpecRep a

  toSpecRep = toSpecRep . fromCompact

instance SpecTranslate ctx (CommitteeAuthorization c) where
  type
    SpecRep (CommitteeAuthorization c) =
      SpecRep (Maybe (Credential 'HotCommitteeRole c))

  toSpecRep (CommitteeHotCredential c) = toSpecRep $ Just c
  toSpecRep (CommitteeMemberResigned _) =
    toSpecRep $
      Nothing @(Credential 'HotCommitteeRole c)

instance SpecTranslate ctx (CommitteeState era) where
  type
    SpecRep (CommitteeState era) =
      SpecRep
        ( Map
            (Credential 'ColdCommitteeRole (EraCrypto era))
            (CommitteeAuthorization (EraCrypto era))
        )

  toSpecRep = toSpecRep . csCommitteeCreds

instance SpecTranslate ctx (IndividualPoolStake c) where
  type SpecRep (IndividualPoolStake c) = SpecRep Coin

  toSpecRep (IndividualPoolStake _ c _) = toSpecRep c

instance SpecTranslate ctx (PoolDistr c) where
  type SpecRep (PoolDistr c) = Agda.HSMap Agda.VDeleg Agda.Coin

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
      SpecRep [(GovActionId (EraCrypto era), GovActionState era)]

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
instance (IsConwayUniv fn, Era era) => HasSpec fn (EpochExecEnv era)

instance SpecTranslate ctx (EpochExecEnv era) where
  type SpecRep (EpochExecEnv era) = ()

  toSpecRep _ = pure ()

-- | This type is used as the Env only in the Agda Spec
data ConwayExecEnactEnv era = ConwayExecEnactEnv
  { ceeeGid :: !(GovActionId (EraCrypto era))
  , ceeeTreasury :: !Coin
  , ceeeEpoch :: !EpochNo
  }
  deriving (Generic, Eq, Show)

-- | Here we inject the Agda Spec Env into the STS rule Environment, which is ().
instance Inject (ConwayExecEnactEnv era) () where
  inject _ = ()

instance ToExpr (ConwayExecEnactEnv era)
instance Era era => NFData (ConwayExecEnactEnv era)

instance HasSimpleRep (ConwayExecEnactEnv era)
instance (IsConwayUniv fn, Era era) => HasSpec fn (ConwayExecEnactEnv era)

instance SpecTranslate ctx (ConwayExecEnactEnv era) where
  type SpecRep (ConwayExecEnactEnv era) = Agda.EnactEnv

  toSpecRep ConwayExecEnactEnv {..} =
    Agda.MkEnactEnv
      <$> toSpecRep ceeeGid
      <*> toSpecRep ceeeTreasury
      <*> toSpecRep ceeeEpoch

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization c ->
  StrictMaybe (Credential 'HotCommitteeRole c)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance HasSimpleRep (DepositPurpose c)
instance (IsConwayUniv fn, Crypto c) => HasSpec fn (DepositPurpose c)

instance SpecTranslate ctx (DepositPurpose c) where
  type SpecRep (DepositPurpose c) = Agda.DepositPurpose
  toSpecRep (CredentialDeposit cred) =
    Agda.CredentialDeposit <$> toSpecRep cred
  toSpecRep (PoolDeposit kh) =
    Agda.PoolDeposit <$> toSpecRep kh
  toSpecRep (DRepDeposit cred) =
    Agda.DRepDeposit <$> toSpecRep cred
  toSpecRep (GovActionDeposit gid) =
    Agda.GovActionDeposit <$> toSpecRep gid
