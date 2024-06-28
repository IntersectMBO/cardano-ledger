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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (
  SpecTranslate (..),
  SpecTranslationError,
  ConwayExecEnactEnv (..),
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Crypto.Util (bytesToNatural)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..), serialiseAddr)
import Cardano.Ledger.Alonzo (AlonzoTxAuxData, MaryValue)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.CertState (CommitteeAuthorization (..), CommitteeState (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Compactible (Compactible)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD (..))
import Cardano.Ledger.Conway.Rules (
  CertEnv (..),
  ConwayCertPredFailure,
  ConwayDelegEnv (..),
  ConwayDelegPredFailure,
  ConwayGovCertEnv (..),
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayNewEpochPredFailure,
  ConwayUtxoPredFailure,
  EnactSignal (..),
  GovEnv (..),
  GovSignal (..),
 )
import Cardano.Ledger.Conway.Scripts (AlonzoScript, ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayGovCert (..),
  ConwayTxCert (..),
  getStakePoolDelegatee,
  getVoteDelegatee,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..), Stake (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus (CostModels, ExUnits (..), Prices)
import Cardano.Ledger.Plutus.Data (BinaryData, Data, Datum (..), hashBinaryData)
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (Identity, PoolEnv (..), ShelleyPoolPredFailure, UtxoEnv (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UMap (dRepMap, fromCompact, rewardMap, sPoolMap)
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
import Data.Map.Strict (Map, mapKeys)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict (OMap, assocList)
import Data.OSet.Strict (OSet)
import Data.Ratio (denominator, numerator)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Traversable (forM)
import qualified Data.VMap as VMap
import Data.Void (Void, absurd)
import Data.Word (Word32, Word64)
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
 )
import Test.Cardano.Ledger.Constrained.Conway (IsConwayUniv)
import Test.Cardano.Ledger.Constrained.Conway.Epoch
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..), showExpr)

instance SpecTranslate ctx Void where
  type SpecRep Void = Agda.AgdaEmpty

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

instance SpecTranslate ctx (Addr era) where
  type SpecRep (Addr era) = Agda.Addr

  toSpecRep = pure . toInteger . bytesToNatural . serialiseAddr

instance SpecTranslate ctx (SafeHash c EraIndependentData) where
  type SpecRep (SafeHash c EraIndependentData) = Agda.DataHash

  toSpecRep _ = pure ()

instance SpecTranslate ctx (SafeHash c EraIndependentScriptIntegrity) where
  type SpecRep (SafeHash c EraIndependentScriptIntegrity) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate ctx (SafeHash c EraIndependentTxBody) where
  type SpecRep (SafeHash c EraIndependentTxBody) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

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
  Script era ~ AlonzoScript era =>
  SpecTranslate ctx (AlonzoScript era)
  where
  type SpecRep (AlonzoScript era) = Agda.DataHash

  toSpecRep _ = pure ()

instance
  ( EraTxOut era
  , SpecRep (Script era) ~ Agda.DataHash
  , SpecRep (Value era) ~ Agda.Coin
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
  ) =>
  SpecTranslate ctx (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState

  toSpecRep x =
    Agda.MkUTxOState
      <$> toSpecRep (utxosUtxo x)
      <*> toSpecRep (utxosFees x)

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

  toSpecRep x =
    Agda.MkPParams
      <$> toSpecRep (cppMinFeeA x)
      <*> toSpecRep (cppMinFeeB x)
      <*> pure (toInteger . unTHKD $ cppMaxBBSize x)
      <*> pure (toInteger . unTHKD $ cppMaxTxSize x)
      <*> pure (toInteger . unTHKD $ cppMaxBHSize x)
      <*> pure (toInteger . unTHKD $ cppMaxValSize x)
      <*> pure 0 -- minUTxOValue has been deprecated and is not supported in Conway
      <*> toSpecRep (cppPoolDeposit x)
      <*> toSpecRep (cppKeyDeposit x)
      <*> toSpecRep (cppEMax x)
      <*> toSpecRep (toInteger . unTHKD $ cppNOpt x)
      <*> toSpecRep (cppProtocolVersion x)
      <*> toSpecRep (cppPoolVotingThresholds x)
      <*> toSpecRep (cppDRepVotingThresholds x)
      <*> toSpecRep (cppGovActionLifetime x)
      <*> toSpecRep (cppGovActionDeposit x)
      <*> toSpecRep (cppDRepDeposit x)
      <*> toSpecRep (cppDRepActivity x)
      <*> pure (toInteger . unTHKD $ cppCommitteeMinSize x)
      <*> pure (toInteger . unEpochInterval . unTHKD $ cppCommitteeMaxTermLength x)
      <*> toSpecRep (cppCostModels x)
      <*> toSpecRep (cppPrices x)
      <*> toSpecRep (cppMaxTxExUnits x)
      <*> toSpecRep (cppMaxBlockExUnits x)
      <*> toSpecRep (cppCoinsPerUTxOByte x)
      <*> pure (toInteger . unTHKD $ cppMaxCollateralInputs x)

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
      <*> toSpecRep (Coin 10000000) -- TODO: Fix generating types

instance SpecTranslate ctx a => SpecTranslate ctx (Set a) where
  type SpecRep (Set a) = Agda.HSSet (SpecRep a)

  toSpecRep = fmap Agda.MkHSSet . traverse toSpecRep . Set.toList

instance SpecTranslate ctx a => SpecTranslate ctx (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate ctx a => SpecTranslate ctx (Sized a) where
  type SpecRep (Sized a) = SpecRep a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate ctx ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance SpecTranslate ctx (Hash a b) where
  type SpecRep (Hash a b) = Agda.Hash
  toSpecRep = pure . hashToInteger

deriving instance SpecTranslate ctx (KeyHash r c)

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
  ) =>
  SpecTranslate ctx (AlonzoTxWits era)
  where
  type SpecRep (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> toSpecRep (toList $ txwitsVKey x)
      <*> pure []
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)

instance SpecTranslate ctx a => SpecTranslate ctx (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate ctx a => SpecTranslate ctx (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)

  toSpecRep = traverse toSpecRep

instance SpecTranslate ctx (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep _ = pure ()

deriving instance SpecTranslate ctx (ScriptHash c)

instance SpecTranslate ctx (Credential k c) where
  type SpecRep (Credential k c) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate ctx Network where
  type SpecRep Network = ()

  toSpecRep = pure . const ()

instance SpecTranslate ctx (RewardAccount c) where
  type SpecRep (RewardAccount c) = Agda.RwdAddr

  toSpecRep (RewardAccount n c) = toSpecRep (n, c)

instance SpecTranslate ctx (PoolParams era) where
  type SpecRep (PoolParams era) = Agda.PoolParams

  toSpecRep PoolParams {..} = toSpecRep $ KeyHashObj ppId

instance SpecTranslate ctx (DRep c) where
  type SpecRep (DRep c) = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.CredVoter Agda.DRep <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.AbstainRep
  toSpecRep DRepAlwaysNoConfidence = pure Agda.NoConfidenceRep

instance SpecTranslate ctx (Anchor c) where
  type SpecRep (Anchor c) = Agda.Anchor
  toSpecRep _ = pure ()

instance SpecTranslate ctx (ConwayTxCert era) where
  type SpecRep (ConwayTxCert era) = Agda.TxCert

  toSpecRep (ConwayTxCertPool p) = toSpecRep p
  toSpecRep (ConwayTxCertGov c) = toSpecRep c
  toSpecRep (ConwayTxCertDeleg x) = toSpecRep x

instance SpecTranslate ctx (PoolCert c) where
  type SpecRep (PoolCert c) = Agda.TxCert

  toSpecRep (RegPool p@PoolParams {ppId = KeyHash ppHash}) =
    Agda.RegPool
      <$> toSpecRep ppHash
      <*> toSpecRep p
  toSpecRep (RetirePool (KeyHash ppHash) e) =
    Agda.RetirePool
      <$> toSpecRep ppHash
      <*> toSpecRep e

instance SpecTranslate ctx (ConwayGovCert c) where
  type SpecRep (ConwayGovCert c) = Agda.TxCert

  toSpecRep (ConwayRegDRep c d _) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> toSpecRep d
      <*> pure ()
  toSpecRep (ConwayUnRegDRep c _) =
    Agda.DeRegDRep
      <$> toSpecRep c
  toSpecRep (ConwayUpdateDRep c _) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> pure 0
      <*> pure ()
  toSpecRep (ConwayAuthCommitteeHotKey c h) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SJust h)
  toSpecRep (ConwayResignCommitteeColdKey c _) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SNothing @(Credential _ _))

instance SpecTranslate ctx (TxId era) where
  type SpecRep (TxId era) = Agda.TxId

  toSpecRep (TxId x) = Agda.MkTxId <$> toSpecRep x

toAgdaTxBody ::
  ( SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , EraTx era
  , BabbageEraTxBody era
  , SpecTranslate ctx (TxOut era)
  , SpecTranslate ctx (TxCert era)
  ) =>
  Tx era ->
  SpecTransM ctx Agda.TxBody
toAgdaTxBody tx =
  Agda.MkTxBody
    <$> toSpecRep (toList $ tx ^. bodyTxL . inputsTxBodyL)
    <*> toSpecRep (toList $ tx ^. bodyTxL . referenceInputsTxBodyL)
    <*> (Agda.MkHSMap . zip [0 ..] <$> toSpecRep (tx ^. bodyTxL . outputsTxBodyL))
    <*> toSpecRep (tx ^. bodyTxL . feeTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . vldtTxBodyL)
    <*> pure (tx ^. sizeTxF)
    <*> toSpecRep (txIdTx tx)
    <*> toSpecRep (toList $ tx ^. bodyTxL . collateralInputsTxBodyL)
    <*> toSpecRep (toList $ tx ^. bodyTxL . reqSignerHashesTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . certsTxBodyL)

instance
  ( SpecTranslate ctx (TxWits era)
  , SpecTranslate ctx (TxAuxData era)
  , SpecTranslate ctx (TxOut era)
  , SpecTranslate ctx (TxCert era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , Tx era ~ AlonzoTx era
  , EraTx era
  , BabbageEraTxBody era
  ) =>
  SpecTranslate ctx (AlonzoTx era)
  where
  type SpecRep (AlonzoTx era) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> toAgdaTxBody @era tx
      <*> toSpecRep (wits tx)
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

  toSpecRep x = pure (numerator r, denominator r)
    where
      r = unboundRational x

instance SpecTranslate ctx (Committee era) where
  type SpecRep (Committee era) = (Agda.HSMap Agda.Credential Agda.Epoch, Agda.Rational)

  toSpecRep (Committee members threshold) = toSpecRep (members, threshold)

instance SpecTranslate ctx (Constitution era) where
  type SpecRep (Constitution era) = (Agda.DataHash, Maybe Agda.ScriptHash)

  toSpecRep (Constitution anchor policy) = toSpecRep (anchor, policy)

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
          pure (((), agdaCred), amount)
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (Agda.MkTxId 0, 0)
        pure (committee, agdaLastId)

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , Inject ctx (Proposals era)
  , EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx (GovEnv era)
  where
  type SpecRep (GovEnv era) = Agda.GovEnv

  toSpecRep GovEnv {..} = do
    props <- askCtx @(Proposals era)
    enactState <- askCtx @(EnactState era)
    let
      protVer = gePParams ^. ppProtocolVersionL
      -- TODO express this in agdaConstraints rather than doing it during
      -- translation
      enactStatePV =
        enactState
          & ensProtVerL .~ protVer
          & ensPrevGovActionIdsL .~ toPrevGovActionIds (props ^. pRootsL)
    Agda.MkGovEnv
      <$> toSpecRep geTxId
      <*> toSpecRep geEpoch
      <*> toSpecRep gePParams
      <*> toSpecRep gePPolicy
      <*> toSpecRep enactStatePV

instance SpecTranslate ctx (Voter era) where
  type SpecRep (Voter era) = Agda.Voter

  toSpecRep (CommitteeVoter c) = (Agda.CC,) <$> toSpecRep c
  toSpecRep (DRepVoter c) = (Agda.DRep,) <$> toSpecRep c
  toSpecRep (StakePoolVoter kh) = (Agda.SPO,) <$> toSpecRep (KeyHashObj kh)

instance SpecTranslate ctx Vote where
  type SpecRep Vote = Agda.Vote

  toSpecRep VoteYes = pure Agda.VoteYes
  toSpecRep VoteNo = pure Agda.VoteNo
  toSpecRep Abstain = pure Agda.VoteAbstain

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
          <$> ( Agda.MkGovVote
                  <$> toSpecRep gaId
                  <*> toSpecRep voter
                  <*> toSpecRep (vProcVote votingProcedure)
                  <*> toSpecRep (vProcAnchor votingProcedure)
              )
          <*> m

instance SpecTranslate ctx (ConwayPParams StrictMaybe era) where
  type SpecRep (ConwayPParams StrictMaybe era) = Agda.PParamsUpdate

  toSpecRep x =
    Agda.MkPParamsUpdate
      <$> toSpecRep (cppMinFeeA x)
      <*> toSpecRep (cppMinFeeB x)
      <*> pure (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxBBSize x)
      <*> pure (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxTxSize x)
      <*> pure (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxBHSize x)
      <*> pure (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxValSize x)
      <*> pure Nothing -- minUTxOValue has been deprecated and is not supported in Conway
      <*> toSpecRep (cppPoolDeposit x)
      <*> toSpecRep (cppKeyDeposit x)
      <*> toSpecRep (cppEMax x)
      <*> toSpecRep (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppNOpt x)
      <*> pure Nothing
      <*> toSpecRep (cppPoolVotingThresholds x)
      <*> toSpecRep (cppDRepVotingThresholds x)
      <*> toSpecRep (cppGovActionLifetime x)
      <*> toSpecRep (cppGovActionDeposit x)
      <*> toSpecRep (cppDRepDeposit x)
      <*> toSpecRep (cppDRepActivity x)
      <*> pure (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppCommitteeMinSize x)
      <*> pure
        (fmap (toInteger . unEpochInterval) . strictMaybeToMaybe . unTHKD $ cppCommitteeMaxTermLength x)
      <*> toSpecRep (cppCostModels x)
      <*> toSpecRep (cppPrices x)
      <*> toSpecRep (cppMaxTxExUnits x)
      <*> toSpecRep (cppMaxBlockExUnits x)
      <*> toSpecRep (cppCoinsPerUTxOByte x)
      <*> pure (fmap toInteger . strictMaybeToMaybe . unTHKD $ cppMaxCollateralInputs x)

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
    Agda.NewCommittee
      <$> toSpecRep add
      <*> toSpecRep (toList remove)
      <*> toSpecRep threshold
  toSpecRep (NewConstitution _ (Constitution anchor policy)) =
    Agda.NewConstitution
      <$> toSpecRep anchor
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

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . assocList

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

instance
  ( EraPParams era
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  ) =>
  SpecTranslate ctx (GovSignal era)
  where
  type SpecRep (GovSignal era) = [Agda.GovSignal]

  toSpecRep GovSignal {gsVotingProcedures, gsProposalProcedures} = do
    votingProcedures <- toSpecRep gsVotingProcedures
    proposalProcedures <- toSpecRep gsProposalProcedures
    pure $
      mconcat
        [ Agda.GovSignalVote <$> votingProcedures
        , Agda.GovSignalProposal <$> proposalProcedures
        ]

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

  toSpecRep = toSpecRep . view pPropsL

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

instance SpecTranslate ctx (ConwayGovCertPredFailure era) where
  type SpecRep (ConwayGovCertPredFailure era) = OpaqueErrorString

  toSpecRep = pure . OpaqueErrorString . showExpr

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  ) =>
  SpecTranslate ctx (CertEnv era)
  where
  type SpecRep (CertEnv era) = Agda.CertEnv

  toSpecRep CertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
    Agda.MkCertEnv
      <$> toSpecRep ceCurrentEpoch
      <*> toSpecRep cePParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , Inject ctx (VotingProcedures era)
  , Inject ctx (Map (Network, Credential 'Staking (EraCrypto era)) Coin)
  ) =>
  SpecTranslate ctx (ConwayGovCertEnv era)
  where
  type SpecRep (ConwayGovCertEnv era) = Agda.CertEnv

  toSpecRep ConwayGovCertEnv {..} = do
    votes <- askCtx @(VotingProcedures era)
    withdrawals <- askCtx @(Map (Network, Credential 'Staking (EraCrypto era)) Coin)
    Agda.MkCertEnv
      <$> toSpecRep cgceCurrentEpoch
      <*> toSpecRep cgcePParams
      <*> toSpecRep votes
      <*> toSpecRep withdrawals

instance SpecTranslate ctx (DState era) where
  type SpecRep (DState era) = Agda.DState

  toSpecRep DState {..} =
    Agda.MkDState
      <$> toSpecRep (dRepMap dsUnified)
      <*> toSpecRep (sPoolMap dsUnified)
      <*> toSpecRep (rewardMap dsUnified)

instance SpecTranslate ctx (PState era) where
  type SpecRep (PState era) = Agda.PState

  toSpecRep PState {..} =
    Agda.MkPState
      <$> toSpecRep (mapKeys (hashToInteger . unKeyHash) psStakePoolParams)
      <*> toSpecRep (mapKeys (hashToInteger . unKeyHash) psRetiring)

committeeCredentialToStrictMaybe ::
  CommitteeAuthorization c ->
  StrictMaybe (Credential 'HotCommitteeRole c)
committeeCredentialToStrictMaybe (CommitteeHotCredential c) = SJust c
committeeCredentialToStrictMaybe (CommitteeMemberResigned _) = SNothing

instance SpecTranslate ctx (VState era) where
  type SpecRep (VState era) = Agda.GState

  toSpecRep VState {..} =
    Agda.MkGState
      <$> toSpecRep (drepExpiry <$> vsDReps)
      <*> toSpecRep
        (committeeCredentialToStrictMaybe <$> csCommitteeCreds vsCommitteeState)

instance SpecTranslate ctx (CertState era) where
  type SpecRep (CertState era) = Agda.CertState

  toSpecRep CertState {..} =
    Agda.MkCertState
      <$> toSpecRep certDState
      <*> toSpecRep certPState
      <*> toSpecRep certVState

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

instance
  Inject ctx Coin =>
  SpecTranslate ctx (RatifyEnv era)
  where
  type SpecRep (RatifyEnv era) = Agda.RatifyEnv

  toSpecRep RatifyEnv {..} = do
    let
      transStakeDistr (c, s) = do
        c' <- toSpecRep c
        s' <- toSpecRep s
        pure (Agda.CredVoter Agda.SPO c', s')
      stakeDistrsMap = traverse transStakeDistr $ Map.toList reStakeDistr
      stakeDistrs = Agda.MkStakeDistrs . Agda.MkHSMap <$> stakeDistrsMap
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

data ConwayExecEnactEnv era = ConwayExecEnactEnv
  { ceeeGid :: GovActionId (EraCrypto era)
  , ceeeTreasury :: Coin
  , ceeeEpoch :: EpochNo
  }
  deriving (Generic, Eq, Show)

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

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (ConwayDelegEnv era)
  where
  type SpecRep (ConwayDelegEnv era) = Agda.DelegEnv

  toSpecRep ConwayDelegEnv {..} =
    Agda.MkDelegEnv
      <$> toSpecRep cdePParams
      <*> toSpecRep (Map.mapKeys (hashToInteger . unKeyHash) cdePools)

instance SpecTranslate ctx (ConwayDelegCert c) where
  type SpecRep (ConwayDelegCert c) = Agda.TxCert

  toSpecRep (ConwayRegCert _ _) = throwError "RegCert not supported"
  toSpecRep (ConwayUnRegCert c _) =
    Agda.Dereg <$> toSpecRep c
  toSpecRep (ConwayDelegCert c d) =
    Agda.Delegate
      <$> toSpecRep c
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (ConwayRegDelegCert s d c) =
    Agda.Delegate
      <$> toSpecRep s
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (hashToInteger . unKeyHash <$> getStakePoolDelegatee d)
      <*> toSpecRep c

instance SpecTranslate ctx (ConwayDelegPredFailure era) where
  type SpecRep (ConwayDelegPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e

instance
  ( SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD Identity era)
  ) =>
  SpecTranslate ctx (PoolEnv era)
  where
  type SpecRep (PoolEnv era) = Agda.PParams

  toSpecRep (PoolEnv _ pp) = toSpecRep pp

instance SpecTranslate ctx (ShelleyPoolPredFailure era) where
  type SpecRep (ShelleyPoolPredFailure era) = OpaqueErrorString

  toSpecRep e = pure . OpaqueErrorString . show $ toExpr e

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , Inject ctx [GovActionState era]
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecTranslate ctx (TxOut era)
  ) =>
  SpecTranslate ctx (EpochState era)
  where
  type SpecRep (EpochState era) = Agda.EpochState

  toSpecRep (EpochState {esLState = esLState@LedgerState {lsUTxOState}, ..}) =
    Agda.MkEpochState
      <$> toSpecRep esAccountState
      <*> toSpecRep esSnapshots
      <*> toSpecRep esLState
      <*> toSpecRep enactState
      <*> toSpecRep ratifyState
    where
      enactState = mkEnactState $ utxosGovState lsUTxOState
      ratifyState = RatifyState enactState mempty mempty False

instance SpecTranslate ctx (SnapShots c) where
  type SpecRep (SnapShots c) = Agda.Snapshots

  toSpecRep (SnapShots {..}) =
    Agda.MkSnapshots
      <$> toSpecRep ssStakeMark
      <*> toSpecRep ssStakeSet
      <*> toSpecRep ssStakeGo
      <*> toSpecRep ssFee

instance SpecTranslate ctx (SnapShot c) where
  type SpecRep (SnapShot c) = Agda.Snapshot

  toSpecRep (SnapShot {..}) =
    Agda.MkSnapshot
      <$> toSpecRep ssStake
      <*> toSpecRep (VMap.toMap ssDelegations)

instance SpecTranslate ctx (Stake c) where
  type SpecRep (Stake c) = Agda.HSMap Agda.Credential Agda.Coin

  toSpecRep (Stake stake) = toSpecRep $ VMap.toMap stake

instance SpecTranslate ctx AccountState where
  type SpecRep AccountState = Agda.Acnt

  toSpecRep (AccountState {..}) =
    Agda.MkAcnt
      <$> toSpecRep asTreasury
      <*> toSpecRep asReserves

instance
  ( ConwayEraGov era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecTranslate ctx (TxOut era)
  ) =>
  SpecTranslate ctx (LedgerState era)
  where
  type SpecRep (LedgerState era) = Agda.LedgerState

  toSpecRep (LedgerState {..}) =
    Agda.MkLedgerState
      <$> toSpecRep lsUTxOState
      <*> toSpecRep (utxosGovState lsUTxOState ^. proposalsGovStateL)
      <*> toSpecRep lsCertState

instance
  ( EraPParams era
  , ConwayEraGov era
  , SpecTranslate ctx (PParamsHKD Identity era)
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  , SpecTranslate ctx (PParamsHKD StrictMaybe era)
  , SpecRep (PParamsHKD StrictMaybe era) ~ Agda.PParamsUpdate
  , Inject ctx [GovActionState era]
  , ToExpr (PParamsHKD StrictMaybe era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecTranslate ctx (TxOut era)
  ) =>
  SpecTranslate ctx (NewEpochState era)
  where
  type SpecRep (NewEpochState era) = Agda.NewEpochState

  toSpecRep (NewEpochState {..}) =
    Agda.MkNewEpochState
      <$> toSpecRep nesEL
      <*> toSpecRep nesEs

instance SpecTranslate ctx (ConwayNewEpochPredFailure (ConwayEra StandardCrypto)) where
  type SpecRep (ConwayNewEpochPredFailure (ConwayEra StandardCrypto)) = OpaqueErrorString

  toSpecRep = pure . OpaqueErrorString . show . toExpr
