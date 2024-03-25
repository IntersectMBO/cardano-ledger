{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Conformance.SpecTranslate
  ( SpecTranslate (..)
  , SpecTranslationError
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
  EpochInterval (..),
  EpochNo (..),
  ProtVer (..),
  SlotNo (..),
  StrictMaybe (..),
  TxIx (..),
  getVersion,
  strictMaybeToMaybe,
 )
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD (..))
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
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), VKey (..))
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Plutus (CostModels, ExUnits (..), Prices)
import Cardano.Ledger.Plutus.Data (Data, Datum (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (SafeHash, extractHash)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
import Cardano.Ledger.Shelley.Rules (Identity, UtxoEnv (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Val (Val (..))
import Data.Bitraversable (bimapM)
import Data.Data (Typeable)
import Data.Foldable (Foldable (..))
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word32, Word64)
import Lens.Micro
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (SpecTranslate (..), SpecTranslationError)
import Cardano.Ledger.Tools (byteStringToNum)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)

instance SpecTranslate TxIx where
  type SpecRep TxIx = Integer

  toSpecRep (TxIx x) = pure $ toInteger x

instance SpecTranslate (TxIn era) where
  type SpecRep (TxIn era) = Agda.TxIn

  toSpecRep (TxIn txId txIx) = toSpecRep (txId, txIx)

instance SpecTranslate (Addr era) where
  type SpecRep (Addr era) = Agda.Addr

  toSpecRep = pure . byteStringToNum . serialiseAddr

instance SpecTranslate (SafeHash c EraIndependentData) where
  type SpecRep (SafeHash c EraIndependentData) = Agda.DataHash

  toSpecRep _ = pure ()

instance SpecTranslate (SafeHash c EraIndependentScriptIntegrity) where
  type SpecRep (SafeHash c EraIndependentScriptIntegrity) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance SpecTranslate (SafeHash c EraIndependentTxBody) where
  type SpecRep (SafeHash c EraIndependentTxBody) = Agda.Hash

  toSpecRep = toSpecRep . extractHash

instance EraTxOut era => SpecTranslate (BabbageTxOut era) where
  type SpecRep (BabbageTxOut era) = Agda.TxOut

  toSpecRep (BabbageTxOut addr val datum _) =
    let mkTxOut x = do
          addr' <- toSpecRep addr
          coin' <- toSpecRep $ coin val
          pure (addr', (coin', x))
     in case datum of
          NoDatum -> mkTxOut Nothing
          DatumHash h -> mkTxOut . Just =<< toSpecRep h
          Datum _ -> Left "Inline datums not supported by spec"

instance SpecTranslate Integer where
  type SpecRep Integer = Integer

  toSpecRep = pure

deriving instance SpecTranslate Coin

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate (UTxO era)
  where
  type SpecRep (UTxO era) = SpecRep (Map (TxIn (EraCrypto era)) (TxOut era))
  toSpecRep (UTxO m) = toSpecRep m
  specToTestRep = L.sortOn fst

instance
  ( SpecTranslate (TxOut era)
  , SpecRep (TxOut era) ~ Agda.TxOut
  ) =>
  SpecTranslate (UTxOState era)
  where
  type SpecRep (UTxOState era) = Agda.UTxOState

  toSpecRep x =
    Agda.MkUTxOState
      <$> toSpecRep (utxosUtxo x)
      <*> toSpecRep (utxosFees x)

  specToTestRep x@(Agda.MkUTxOState _ _) =
    let Agda.MkUTxOState {..} = x
     in Agda.MkUTxOState (specToTestRep @(UTxO era) utxo) (specToTestRep @Coin fees)

deriving instance SpecTranslate SlotNo

deriving instance SpecTranslate EpochNo

deriving instance SpecTranslate EpochInterval

instance SpecTranslate ProtVer where
  type SpecRep ProtVer = (Integer, Integer)

  toSpecRep (ProtVer ver minor) = pure (getVersion ver, toInteger minor)

data VotingThresholds
  = VotingThresholds
      DRepVotingThresholds
      PoolVotingThresholds

instance SpecTranslate VotingThresholds where
  type SpecRep VotingThresholds = ()

  toSpecRep _ = pure ()

instance SpecTranslate CostModels where
  type SpecRep CostModels = Agda.AgdaEmpty

  toSpecRep _ =
    pure $
      error "TODO change this to unit once it is fixed in the spec"

instance SpecTranslate Prices where
  type SpecRep Prices = ()

  toSpecRep _ = pure ()

instance SpecTranslate ExUnits where
  type SpecRep ExUnits = Agda.ExUnits

  toSpecRep (ExUnits a b) = pure (toInteger a, toInteger b)

deriving instance SpecTranslate OrdExUnits

deriving instance SpecTranslate CoinPerByte

instance SpecTranslate (HKD f a) => SpecTranslate (THKD r f a) where
  type SpecRep (THKD r f a) = SpecRep (HKD f a)

  toSpecRep = toSpecRep . unTHKD

instance SpecTranslate (ConwayPParams Identity era) where
  type SpecRep (ConwayPParams Identity era) = Agda.PParams

  toSpecRep x =
    Agda.MkPParams
      <$> toSpecRep (cppMinFeeA x)
      <*> toSpecRep (cppMinFeeB x)
      <*> Right (toInteger . unTHKD $ cppMaxBBSize x)
      <*> Right (toInteger . unTHKD $ cppMaxTxSize x)
      <*> Right (toInteger . unTHKD $ cppMaxBHSize x)
      <*> Right (toInteger . unTHKD $ cppMaxValSize x)
      <*> Right 0
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
      <*> Right (toInteger . unTHKD $ cppCommitteeMinSize x)
      <*> Right (toInteger . unEpochInterval . unTHKD $ cppCommitteeMaxTermLength x)
      <*> toSpecRep (cppCostModels x)
      <*> toSpecRep (cppPrices x)
      <*> toSpecRep (cppMaxTxExUnits x)
      <*> toSpecRep (cppMaxBlockExUnits x)
      <*> toSpecRep (cppCoinsPerUTxOByte x)
      <*> Right (toInteger . unTHKD $ cppMaxCollateralInputs x)

instance
  SpecTranslate (PParamsHKD Identity era) =>
  SpecTranslate (PParams era)
  where
  type SpecRep (PParams era) = SpecRep (PParamsHKD Identity era)

  toSpecRep (PParams x) = toSpecRep x

instance
  ( SpecTranslate (PParams era)
  , SpecRep (PParams era) ~ Agda.PParams
  ) =>
  SpecTranslate (UtxoEnv era)
  where
  type SpecRep (UtxoEnv era) = Agda.UTxOEnv

  toSpecRep x =
    Agda.MkUTxOEnv
      <$> toSpecRep (ueSlot x)
      <*> toSpecRep (uePParams x)

instance (SpecTranslate a, Ord (SpecRep a)) => SpecTranslate (Set a) where
  type SpecRep (Set a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . Set.toList
  specToTestRep = L.sort
  toTestRep = toSpecRep

instance SpecTranslate a => SpecTranslate (StrictSeq a) where
  type SpecRep (StrictSeq a) = [SpecRep a]

  toSpecRep = traverse toSpecRep . toList

instance SpecTranslate a => SpecTranslate (Sized a) where
  type SpecRep (Sized a) = SpecRep a

  toSpecRep (Sized x _) = toSpecRep x

instance SpecTranslate ValidityInterval where
  type SpecRep ValidityInterval = (Maybe Integer, Maybe Integer)

  toSpecRep (ValidityInterval lo hi) = toSpecRep (lo, hi)

instance SpecTranslate (Hash a b) where
  type SpecRep (Hash a b) = Agda.Hash

  toSpecRep = pure . byteStringToNum . hashToBytes

deriving instance SpecTranslate (KeyHash r c)

instance Crypto c => SpecTranslate (VKey k c) where
  type SpecRep (VKey k c) = Integer

  toSpecRep (VKey x) = pure . byteStringToNum $ rawSerialiseVerKeyDSIGN x

instance DSIGNAlgorithm v => SpecTranslate (SignedDSIGN v a) where
  type SpecRep (SignedDSIGN v a) = Integer

  toSpecRep (SignedDSIGN x) =
    pure . byteStringToNum $ rawSerialiseSigDSIGN x

instance (Crypto c, Typeable k) => SpecTranslate (WitVKey k c) where
  type SpecRep (WitVKey k c) = (Integer, Integer)

  toSpecRep (WitVKey vk sk) = toSpecRep (vk, sk)

instance Era era => SpecTranslate (TxDats era) where
  type SpecRep (TxDats era) = [(Agda.DataHash, Agda.Datum)]

  toSpecRep (TxDats x) = toSpecRep x

instance
  ( SpecTranslate k
  , SpecTranslate v
  , Ord (SpecRep k)
  ) =>
  SpecTranslate (Map k v)
  where
  type SpecRep (Map k v) = [(SpecRep k, SpecRep v)]

  toSpecRep = traverse (bimapM toSpecRep toSpecRep) . Map.toList
  specToTestRep = L.sortOn fst
  toTestRep = toSpecRep

instance SpecTranslate Word64 where
  type SpecRep Word64 = Integer

  toSpecRep = pure . toInteger

instance SpecTranslate Word32 where
  type SpecRep Word32 = Integer

  toSpecRep = pure . toInteger

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

instance (SpecTranslate a, SpecTranslate b) => SpecTranslate (a, b) where
  type SpecRep (a, b) = (SpecRep a, SpecRep b)

  toSpecRep (x, y) = (,) <$> toSpecRep x <*> toSpecRep y

instance SpecTranslate (Data era) where
  type SpecRep (Data era) = ()

  toSpecRep _ = pure ()

instance
  ( AlonzoEraScript era
  , Ord (SpecRep (PlutusPurpose AsIx era))
  , SpecTranslate (PlutusPurpose AsIx era)
  ) =>
  SpecTranslate (Redeemers era)
  where
  type
    SpecRep (Redeemers era) =
      [(SpecRep (PlutusPurpose AsIx era), (Agda.Redeemer, Agda.ExUnits))]

  toSpecRep (Redeemers x) = toSpecRep x

instance
  ( AlonzoEraScript era
  , SpecTranslate (PlutusPurpose AsIx era)
  , SpecRep (PlutusPurpose AsIx era) ~ Agda.RdmrPtr
  ) =>
  SpecTranslate (AlonzoTxWits era)
  where
  type SpecRep (AlonzoTxWits era) = Agda.TxWitnesses

  toSpecRep x =
    Agda.MkTxWitnesses
      <$> toSpecRep (txwitsVKey x)
      <*> pure []
      <*> toSpecRep (txdats x)
      <*> toSpecRep (txrdmrs x)

instance SpecTranslate a => SpecTranslate (StrictMaybe a) where
  type SpecRep (StrictMaybe a) = Maybe (SpecRep a)

  toSpecRep = toSpecRep . strictMaybeToMaybe

instance SpecTranslate a => SpecTranslate (Maybe a) where
  type SpecRep (Maybe a) = Maybe (SpecRep a)

  toSpecRep = traverse toSpecRep

instance SpecTranslate (AlonzoTxAuxData era) where
  type SpecRep (AlonzoTxAuxData era) = Agda.AuxiliaryData

  toSpecRep _ = pure ()

deriving instance SpecTranslate (ScriptHash c)

instance SpecTranslate (Credential k c) where
  type SpecRep (Credential k c) = Agda.Credential

  toSpecRep (KeyHashObj h) = Agda.KeyHashObj <$> toSpecRep h
  toSpecRep (ScriptHashObj h) = Agda.ScriptObj <$> toSpecRep h

instance SpecTranslate (RewardAccount c) where
  type SpecRep (RewardAccount c) = SpecRep (Credential 'Staking c)

  toSpecRep = toSpecRep . raCredential

instance SpecTranslate (PoolParams era) where
  type SpecRep (PoolParams era) = Agda.PoolParams

  toSpecRep = toSpecRep . ppRewardAccount

instance SpecTranslate (DRep c) where
  type SpecRep (DRep c) = Agda.VDeleg

  toSpecRep (DRepCredential c) = Agda.CredVoter Agda.DRep <$> toSpecRep c
  toSpecRep DRepAlwaysAbstain = pure Agda.AbstainRep
  toSpecRep DRepAlwaysNoConfidence = pure Agda.NoConfidenceRep

instance SpecTranslate (Anchor c) where
  type SpecRep (Anchor c) = Agda.Anchor
  toSpecRep _ = pure ()

instance SpecTranslate (ConwayTxCert era) where
  type SpecRep (ConwayTxCert era) = Agda.TxCert

  toSpecRep (ConwayTxCertDeleg (ConwayRegCert _ _)) = Left "RegCert not supported"
  toSpecRep (ConwayTxCertDeleg (ConwayUnRegCert _ _)) = Left "UnRegCert not supported"
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

deriving instance SpecTranslate (TxId era)

toAgdaTxBody ::
  ( SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , EraTx era
  , AlonzoEraTxBody era
  , SpecTranslate (TxOut era)
  , SpecTranslate (TxCert era)
  ) =>
  Tx era ->
  Either SpecTranslationError Agda.TxBody
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
  ( SpecTranslate (TxWits era)
  , SpecTranslate (TxAuxData era)
  , SpecTranslate (TxOut era)
  , SpecTranslate (TxCert era)
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxAuxData era) ~ Agda.AuxiliaryData
  , SpecRep (TxOut era) ~ Agda.TxOut
  , SpecRep (TxCert era) ~ Agda.TxCert
  , Tx era ~ AlonzoTx era
  , EraTx era
  , AlonzoEraTxBody era
  ) =>
  SpecTranslate (AlonzoTx era)
  where
  type SpecRep (AlonzoTx era) = Agda.Tx

  toSpecRep tx =
    Agda.MkTx
      <$> toAgdaTxBody @era tx
      <*> toSpecRep (wits tx)
      <*> toSpecRep (auxiliaryData tx)

instance SpecTranslate (BabbageUtxoPredFailure era) where
  type SpecRep (BabbageUtxoPredFailure era) = ()

  toSpecRep _ = pure ()
