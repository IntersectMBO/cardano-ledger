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
  OpaqueErrorString (..),
  GovProceduresSpecTransCtx,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), SignedDSIGN (..))
import Cardano.Crypto.Hash (Hash, hashToBytes)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..), serialiseAddr)
import Cardano.Ledger.Alonzo (AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.PParams (OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..), Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  Anchor,
  BoundedRational (..),
  EpochInterval (..),
  EpochNo (..),
  Inject,
  Network,
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  UnitInterval,
  getVersion,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Committee (Committee),
  Constitution (..),
  EnactState (..),
  GovAction (..),
  GovActionId (GovActionId),
  GovActionIx (..),
  GovActionState (..),
  GovProcedures (..),
  GovPurposeId (GovPurposeId),
  GovRelation (..),
  ProposalProcedure (..),
  Proposals,
  Vote (..),
  Voter (..),
  VotingProcedure (..),
  VotingProcedures (..),
  foldrVotingProcedures,
  gasAction,
  gasReturnAddr,
  pPropsL,
 )
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD (..))
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure,
  ConwayUtxoPredFailure,
  GovEnv (..),
 )
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayGovCert (..),
  ConwayTxCert (..),
  getStakePoolDelegatee,
  getVoteDelegatee,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Keys (KeyHash (..), VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus (CostModels, ExUnits (..), Prices)
import Cardano.Ledger.Plutus.Data (Data, Datum (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (Identity, UtxoEnv (..))
import Cardano.Ledger.Tools (byteStringToNum)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Constrained (HasSimpleRep, HasSpec)
import Control.DeepSeq (NFData)
import Control.Monad.Except (MonadError (..))
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.Data (Typeable)
import Data.Foldable (Foldable (..))
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OMap.Strict (OMap, assocList)
import Data.OSet.Strict (OSet)
import Data.Ratio (denominator, numerator)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (forM)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lens.Micro
import Lens.Micro.Extras (view)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  SpecTransM,
  SpecTranslate (..),
  SpecTranslationError,
  askTransCtx,
 )
import Test.Cardano.Ledger.Constrained.Conway.Instances (IsConwayUniv)
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr (..))

-- | OpaqueErrorString behaves like unit in comparisons, but contains an
-- error string that can be displayed.
newtype OpaqueErrorString = OpaqueErrorString String
  deriving (Generic)

instance Eq OpaqueErrorString where
  _ == _ = True

instance ToExpr OpaqueErrorString

instance NFData OpaqueErrorString

instance SpecTranslate ctx TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate ctx (TxIn era) where
  type SpecRep (TxIn era) = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

instance SpecTranslate ctx (Addr era) where
  type SpecRep (Addr era) = Agda.Addr

  toSpecRep = pure . byteStringToNum . serialiseAddr

instance SpecTranslate ctx (SafeHash c EraIndependentData) where
  type SpecRep (SafeHash c EraIndependentData) = Agda.DataHash

  toSpecRep _ = pure ()

instance SpecTranslate ctx (SafeHash c EraIndependentScriptIntegrity) where
  type SpecRep (SafeHash c EraIndependentScriptIntegrity) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate ctx (SafeHash c EraIndependentTxBody) where
  type SpecRep (SafeHash c EraIndependentTxBody) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance EraTxOut era => SpecTranslate ctx (BabbageTxOut era) where
  type SpecRep (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum _) =
    let mkTxOut x = do
          addr' <- toSpecRep addr
          coin' <- toSpecRep $ coin val
          pure (addr', (coin', x))
     in case datum of
          NoDatum -> mkTxOut Nothing
          DatumHash h -> mkTxOut . Just =<< toSpecRep h
          Datum _ -> throwError "Inline datums not supported by spec"

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
  specToTestRep = L.sortOn fst

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

  specToTestRep x@(Agda.MkUTxOState _ _) =
    let Agda.MkUTxOState {..} = x
     in Agda.MkUTxOState (specToTestRep @ctx @(UTxO era) utxo) (specToTestRep @_ @Coin fees)

deriving instance SpecTranslate ctx SlotNo

deriving instance SpecTranslate ctx EpochNo

deriving instance SpecTranslate ctx EpochInterval

instance SpecTranslate ctx ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

data VotingThresholds
  = VotingThresholds
      DRepVotingThresholds
      PoolVotingThresholds

instance SpecTranslate ctx VotingThresholds where
  type SpecRep VotingThresholds = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx CostModels where
  type SpecRep CostModels = Agda.AgdaEmpty

  toSpecRep _ =
    pure $
      error "TODO change this to unit once it is fixed in the spec"

instance SpecTranslate ctx Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ctx ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

deriving instance SpecTranslate ctx OrdExUnits

deriving instance SpecTranslate ctx CoinPerByte

instance
  ( SpecTranslate ctx (HKD f a)
  , Eq (SpecRep (HKD f a))
  , ToExpr (SpecRep (HKD f a))
  , NFData (SpecRep (HKD f a))
  ) =>
  SpecTranslate ctx (THKD r f a)
  where
  type SpecRep (THKD r f a) = SpecRep (HKD f a)

  toSpecRep = toSpecRep . unTHKD

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
      <*> toSpecRep (cppEMax x)
      <*> toSpecRep (toInteger . unTHKD $ cppNOpt x)
      <*> toSpecRep (cppProtocolVersion x)
      <*> toSpecRep
        ( VotingThresholds
            (unTHKD $ cppDRepVotingThresholds x)
            (unTHKD $ cppPoolVotingThresholds x)
        )
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
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , Eq (SpecRep (PParamsHKD Identity era))
  , ToExpr (SpecRep (PParamsHKD Identity era))
  , NFData (SpecRep (PParamsHKD Identity era))
  ) =>
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

instance (SpecTranslate ctx a, Ord (TestRep a)) => SpecTranslate ctx (Set a) where
  type SpecRep (Set a) = [SpecRep a]
  type TestRep (Set a) = [TestRep a]

  toSpecRep = traverse toSpecRep . Set.toList
  specToTestRep = L.sort . fmap (specToTestRep @ctx @a)

instance SpecTranslate ctx a => SpecTranslate ctx (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]
  type TestRep (StrictSeq a) = [TestRep a]

  toSpecRep = traverse toSpecRep . toList
  specToTestRep = fmap (specToTestRep @ctx @a)

instance SpecTranslate ctx a => SpecTranslate ctx (Sized a) where
  type SpecRep (Sized a) = SpecRep a
  type TestRep (Sized a) = TestRep a

  toSpecRep (Sized x _) = toSpecRep x
  specToTestRep = specToTestRep @ctx @a

instance SpecTranslate ctx ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance SpecTranslate ctx (Hash a b) where
  type SpecRep (Hash a b) = Agda.Hash

  toSpecRep = pure . byteStringToNum . hashToBytes

deriving instance SpecTranslate ctx (KeyHash r c)

instance Crypto c => SpecTranslate ctx (VKey k c) where
  type SpecRep (VKey k c) = Integer

  toSpecRep (VKey x) = pure . byteStringToNum $ rawSerialiseVerKeyDSIGN x

instance DSIGNAlgorithm v => SpecTranslate ctx (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) =
    pure . byteStringToNum $ rawSerialiseSigDSIGN x

instance (Crypto c, Typeable k) => SpecTranslate ctx (WitVKey k c) where
  type SpecRep (WitVKey k c) = (Integer, Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep (vk, sk)

instance Era era => SpecTranslate ctx (TxDats era) where
  type SpecRep (TxDats era) = [(Agda.DataHash, Agda.Datum)]

  toSpecRep (TxDats x) = toSpecRep x

instance
  ( SpecTranslate ctx k
  , SpecTranslate ctx v
  , Ord (TestRep k)
  ) =>
  SpecTranslate ctx (Map k v)
  where
  type SpecRep (Map k v) = [(SpecRep k, SpecRep v)]
  type TestRep (Map k v) = [(TestRep k, TestRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . Map.toList
  specToTestRep = L.sortOn fst . fmap (bimap (specToTestRep @ctx @k) (specToTestRep @ctx @v))

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
  type TestRep (a, b) = (TestRep a, TestRep b)

  toSpecRep (x, y) = (,) <$> toSpecRep x <*> toSpecRep y
  specToTestRep = bimap (specToTestRep @ctx @a) (specToTestRep @ctx @b)

instance SpecTranslate ctx (Data era) where
  type SpecRep (Data era) = ()

  toSpecRep _ = pure ()

instance
  ( AlonzoEraScript era
  , Ord (TestRep (PlutusPurpose AsIx era))
  , SpecTranslate ctx (PlutusPurpose AsIx era)
  ) =>
  SpecTranslate ctx (Redeemers era)
  where
  type
    SpecRep (Redeemers era) =
      [(SpecRep (PlutusPurpose AsIx era), (Agda.Redeemer, Agda.ExUnits))]
  type
    TestRep (Redeemers era) =
      [(TestRep (PlutusPurpose AsIx era), (Agda.Redeemer, Agda.ExUnits))]

  toSpecRep (Redeemers x) = toSpecRep x
  specToTestRep = fmap (first $ specToTestRep @ctx @(PlutusPurpose AsIx era))

instance
  ( AlonzoEraScript era
  , SpecTranslate ctx (PlutusPurpose AsIx era)
  , SpecRep (PlutusPurpose AsIx era) ~ Agda.RdmrPtr
  , Ord (TestRep (PlutusPurpose AsIx era))
  ) =>
  SpecTranslate ctx (AlonzoTxWits era)
  where
  type SpecRep (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> toSpecRep (txwitsVKey x)
      <*> pure []
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)

instance SpecTranslate ctx a => SpecTranslate ctx (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)
  type TestRep (StrictMaybe a) = Maybe (TestRep a)

  toSpecRep = toSpecRep . strictMaybeToMaybe
  specToTestRep = fmap (specToTestRep @ctx @a)

instance SpecTranslate ctx a => SpecTranslate ctx (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)
  type TestRep (Maybe a) = Maybe (TestRep a)

  toSpecRep = traverse toSpecRep
  specToTestRep = fmap (specToTestRep @ctx @a)

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

  toSpecRep (ConwayTxCertDeleg (ConwayRegCert _ _)) = throwError "RegCert not supported"
  toSpecRep (ConwayTxCertDeleg (ConwayUnRegCert _ _)) = throwError "UnRegCert not supported"
  toSpecRep (ConwayTxCertDeleg (ConwayDelegCert c d)) =
    Agda.Delegate
      <$> toSpecRep c
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (KeyHashObj <$> getStakePoolDelegatee d)
      <*> pure 0
  toSpecRep (ConwayTxCertDeleg (ConwayRegDelegCert s d c)) =
    Agda.Delegate
      <$> toSpecRep s
      <*> toSpecRep (getVoteDelegatee d)
      <*> toSpecRep (KeyHashObj <$> getStakePoolDelegatee d)
      <*> toSpecRep c
  toSpecRep (ConwayTxCertPool (RegPool p@PoolParams {ppId})) =
    Agda.RegPool
      <$> toSpecRep (KeyHashObj ppId)
      <*> toSpecRep p
  toSpecRep (ConwayTxCertPool (RetirePool kh e)) =
    Agda.RetirePool
      <$> toSpecRep (KeyHashObj kh)
      <*> toSpecRep e
  toSpecRep (ConwayTxCertGov (ConwayRegDRep c d _)) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> toSpecRep d
      <*> pure () -- TODO Are the anchors supposed to be optional?
  toSpecRep (ConwayTxCertGov (ConwayUnRegDRep c _)) =
    Agda.DeRegDRep
      <$> toSpecRep c
  toSpecRep (ConwayTxCertGov (ConwayUpdateDRep c _)) =
    Agda.RegDRep
      <$> toSpecRep c
      <*> pure 0
      <*> pure () -- TODO Are the anchors supposed to be optional?
  toSpecRep (ConwayTxCertGov (ConwayAuthCommitteeHotKey c h)) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SJust h)
  toSpecRep (ConwayTxCertGov (ConwayResignCommitteeColdKey c _)) =
    Agda.CCRegHot
      <$> toSpecRep c
      <*> toSpecRep (SNothing @(Credential _ _))

deriving instance SpecTranslate ctx (TxId era)

toAgdaTxBody ::
  ( SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , EraTx era
  , AlonzoEraTxBody era
  , SpecTranslate ctx (TxOut era)
  , SpecTranslate ctx (TxCert era)
  ) =>
  Tx era ->
  SpecTransM ctx Agda.TxBody
toAgdaTxBody tx =
  Agda.MkTxBody
    <$> toSpecRep (tx ^. bodyTxL . inputsTxBodyL)
    <*> (zip [0 ..] <$> toSpecRep (tx ^. bodyTxL . outputsTxBodyL))
    <*> toSpecRep (tx ^. bodyTxL . feeTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . vldtTxBodyL)
    <*> pure (tx ^. sizeTxF)
    <*> toSpecRep (txIdTx tx)
    <*> toSpecRep (tx ^. bodyTxL . collateralInputsTxBodyL)
    <*> toSpecRep (tx ^. bodyTxL . reqSignerHashesTxBodyL)
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
  , AlonzoEraTxBody era
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
  type SpecRep UnitInterval = (Integer, Integer)

  toSpecRep x = pure (numerator r, denominator r)
    where
      r = unboundRational x

instance SpecTranslate ctx (Committee era) where
  type SpecRep (Committee era) = ([(Agda.Credential, Agda.Epoch)], Agda.Rational)

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
      transWithdrawals ws = forM (Map.toList ws) $
        \(cred, Coin amount) -> do
          agdaCred <- toSpecRep cred
          pure (((), agdaCred), amount)
      transHashProtected x h = do
        committee <- toSpecRep x
        agdaLastId <- case h of
          SJust lastId -> toSpecRep lastId
          SNothing -> pure (0, 0)
        pure (committee, agdaLastId)

instance
  ( SpecTranslate ctx (PParamsHKD Identity era)
  , Inject ctx (EnactState era)
  , EraPParams era
  , SpecRep (PParamsHKD Identity era) ~ Agda.PParams
  ) =>
  SpecTranslate ctx (GovEnv era)
  where
  type SpecRep (GovEnv era) = Agda.GovEnv
  type SpecTransContext (GovEnv era) = EnactState era

  toSpecRep GovEnv {..} = do
    enactState <- askTransCtx @(GovEnv era)
    Agda.MkGovEnv
      <$> toSpecRep geTxId
      <*> toSpecRep geEpoch
      <*> toSpecRep gePParams
      <*> toSpecRep gePPolicy
      <*> toSpecRep enactState

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
  type SpecRep (VotingProcedures era) = [Agda.GovSignal]

  toSpecRep = foldrVotingProcedures go (pure [])
    where
      go ::
        Voter (EraCrypto era) ->
        GovActionId (EraCrypto era) ->
        VotingProcedure era ->
        SpecTransM ctx [Agda.GovSignal] ->
        SpecTransM ctx [Agda.GovSignal]
      go voter gaId votingProcedure m =
        (:)
          <$> fmap
            Agda.GovSignalVote
            ( Agda.MkGovVote
                <$> toSpecRep gaId
                <*> toSpecRep voter
                <*> toSpecRep (vProcVote votingProcedure)
                <*> toSpecRep (vProcAnchor votingProcedure)
            )
          <*> m

instance SpecTranslate ctx (GovAction era) where
  type SpecRep (GovAction era) = Agda.GovAction

  toSpecRep ParameterChange {} = pure $ Agda.ChangePParams ()
  toSpecRep (HardForkInitiation _ pv) = Agda.TriggerHF <$> toSpecRep pv
  toSpecRep (TreasuryWithdrawals withdrawals _) =
    Agda.TreasuryWdrl
      <$> toSpecRep withdrawals
  toSpecRep (NoConfidence _) = pure Agda.NoConfidence
  toSpecRep (UpdateCommittee _ remove add threshold) =
    Agda.NewCommittee
      <$> toSpecRep add
      <*> toSpecRep remove
      <*> toSpecRep threshold
  toSpecRep (NewConstitution _ (Constitution anchor policy)) =
    Agda.NewConstitution
      <$> toSpecRep anchor
      <*> toSpecRep policy
  toSpecRep InfoAction = pure Agda.Info

toAgdaProposalProcedure ::
  GovActionId (EraCrypto era) ->
  StrictMaybe (ScriptHash (EraCrypto era)) ->
  ProposalProcedure era ->
  SpecTransM ctx Agda.GovSignal
toAgdaProposalProcedure gaId policy ProposalProcedure {..} =
  fmap Agda.GovSignalProposal $
    Agda.MkGovProposal
      <$> toSpecRep pProcGovAction
      <*> toSpecRep gaId
      <*> toSpecRep policy
      <*> toSpecRep pProcDeposit
      <*> toSpecRep pProcReturnAddr
      <*> toSpecRep pProcAnchor

instance SpecTranslate ctx a => SpecTranslate ctx (OSet a) where
  type SpecRep (OSet a) = [SpecRep a]
  type TestRep (OSet a) = [TestRep a]

  toSpecRep = traverse toSpecRep . toList
  specToTestRep = fmap (specToTestRep @ctx @a)

instance
  ( SpecTranslate ctx k
  , SpecTranslate ctx v
  ) =>
  SpecTranslate ctx (OMap k v)
  where
  type SpecRep (OMap k v) = [(SpecRep k, SpecRep v)]
  type TestRep (OMap k v) = [(TestRep k, TestRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . assocList
  specToTestRep = fmap (bimap (specToTestRep @ctx @k) (specToTestRep @ctx @v))

data GovProceduresSpecTransCtx c
  = GovProceduresSpecTransCtx
      (GovActionId c)
      (StrictMaybe (ScriptHash c))
  deriving (Eq, Show, Generic)

instance HasSimpleRep (GovProceduresSpecTransCtx c)

instance (IsConwayUniv fn, Crypto c) => HasSpec fn (GovProceduresSpecTransCtx c)

instance
  Inject ctx (GovProceduresSpecTransCtx (EraCrypto era)) =>
  SpecTranslate ctx (GovProcedures era)
  where
  type SpecRep (GovProcedures era) = [Agda.GovSignal]
  type SpecTransContext (GovProcedures era) = GovProceduresSpecTransCtx (EraCrypto era)

  toSpecRep GovProcedures {..} = do
    GovProceduresSpecTransCtx gaId policy <- askTransCtx @(GovProcedures era)
    (++)
      <$> toSpecRep gpVotingProcedures
      <*> traverse
        (toAgdaProposalProcedure gaId policy)
        (toList gpProposalProcedures)

instance SpecTranslate ctx (GovActionState era) where
  type SpecRep (GovActionState era) = Agda.GovActionState

  toSpecRep gas@GovActionState {..} =
    Agda.MkGovActionState
      <$> agdaVoteMap
      <*> toSpecRep (gasReturnAddr gas)
      <*> toSpecRep gasExpiresAfter
      <*> toSpecRep (gasAction gas)
      <*> toSpecRep gasId
    where
      agdaVoteMap = do
        drepVotes <- toSpecRep gasDRepVotes
        ccVotes <- toSpecRep gasCommitteeVotes
        spoVotes <- toSpecRep gasStakePoolVotes
        pure $
          mconcat
            [ first (Agda.DRep,) <$> drepVotes
            , first (Agda.CC,) <$> ccVotes
            , first (\h -> (Agda.SPO, Agda.KeyHashObj h)) <$> spoVotes
            ]

instance SpecTranslate ctx GovActionIx where
  type SpecRep GovActionIx = Integer

  toSpecRep = pure . fromIntegral . unGovActionIx

instance SpecTranslate ctx (GovActionId c) where
  type SpecRep (GovActionId c) = Agda.GovActionID

  toSpecRep (GovActionId txId gaIx) = toSpecRep (txId, gaIx)

instance SpecTranslate ctx (Proposals era) where
  type SpecRep (Proposals era) = Agda.GovState

  toSpecRep = toSpecRep . view pPropsL
