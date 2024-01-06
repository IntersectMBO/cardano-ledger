{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxInfo (
  ConwayContextError (..),
  transTxBodyWithdrawals,
  transTxCert,
  transDRepCred,
  transColdCommitteeCred,
  transHotCommitteeCred,
  transDelegatee,
  transDRep,
  transScriptPurpose,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (..),
  EraPlutusTxInfo (..),
  PlutusTxCert,
  mkPlutusLanguageContext,
 )
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), TxOutSource (..))
import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Alonzo
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import qualified Cardano.Ledger.Babbage.TxInfo as Babbage
import Cardano.Ledger.BaseTypes (EpochNo (..), Inject (..), kindObject)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Scripts (PlutusScript (..))
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.Plutus.TxInfo (coinToLovelace, transCoin, transCred, transKeyHash, transTxIn)
import Cardano.Ledger.PoolParams
import Control.DeepSeq (NFData)
import Control.Monad (unless, zipWithM)
import Data.Aeson (ToJSON (..), (.=))
import Data.Foldable as F (Foldable (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Lens.Micro
import NoThunks.Class (NoThunks)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3

instance Crypto c => EraPlutusContext (ConwayEra c) where
  type ContextError (ConwayEra c) = ConwayContextError (ConwayEra c)

  mkPlutusScriptContext = \case
    ConwayPlutusV1 p -> mkPlutusLanguageContext p
    ConwayPlutusV2 p -> mkPlutusLanguageContext p
    ConwayPlutusV3 p -> mkPlutusLanguageContext p

data ConwayContextError era
  = BabbageContextError !(BabbageContextError era)
  | CertificateNotSupported !(TxCert era)
  deriving (Generic)

deriving instance (Eq (ContextError era), Eq (TxCert era)) => Eq (ConwayContextError era)

deriving instance (Show (ContextError era), Show (TxCert era)) => Show (ConwayContextError era)

instance Inject (ConwayContextError era) (ConwayContextError era)

instance Inject (BabbageContextError era) (ConwayContextError era) where
  inject = BabbageContextError

instance Inject (AlonzoContextError era) (ConwayContextError era) where
  inject = BabbageContextError . inject

instance NoThunks (TxCert era) => NoThunks (ConwayContextError era)

instance (Era era, NFData (TxCert era)) => NFData (ConwayContextError era)

instance (EncCBOR (TxCert era), Era era) => EncCBOR (ConwayContextError era) where
  encCBOR = \case
    -- We start at tag 8, just in case to avoid clashes with previous eras.
    BabbageContextError babbageContextError ->
      encode $ Sum BabbageContextError 8 !> To babbageContextError
    CertificateNotSupported txCert ->
      encode $ Sum CertificateNotSupported 9 !> To txCert

instance (DecCBOR (TxCert era), Era era) => DecCBOR (ConwayContextError era) where
  decCBOR = decode $ Summands "ContextError" $ \case
    8 -> SumD BabbageContextError <! From
    9 -> SumD CertificateNotSupported <! From
    n -> Invalid n

instance ToJSON (TxCert era) => ToJSON (ConwayContextError era) where
  toJSON = \case
    BabbageContextError err -> toJSON err
    CertificateNotSupported txCert ->
      kindObject "CertificateNotSupported" ["certificate" .= toJSON txCert]

instance Crypto c => EraPlutusTxInfo 'PlutusV1 (ConwayEra c) where
  toPlutusTxCert _ = transTxCertV1V2

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    let refInputs = txBody ^. referenceInputsTxBodyL
    unless (Set.null refInputs) $ Left $ inject $ ReferenceInputsNotSupported @(ConwayEra c) refInputs

    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (Babbage.transTxInInfoV1 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV1 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    pure
      PV1.TxInfo
        { PV1.txInfoInputs = inputs
        , PV1.txInfoOutputs = outputs
        , PV1.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV1.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV1.txInfoDCert = txCerts
        , PV1.txInfoWdrl = Alonzo.transTxBodyWithdrawals txBody
        , PV1.txInfoValidRange = timeRange
        , PV1.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV1.txInfoData = Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV1.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV1.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV2 (ConwayEra c) where
  toPlutusTxCert _ = transTxCertV1V2

  toPlutusScriptPurpose = Alonzo.transScriptPurpose

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (Babbage.transTxInInfoV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (Babbage.transTxInInfoV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy tx
    pure
      PV2.TxInfo
        { PV2.txInfoInputs = inputs
        , PV2.txInfoOutputs = outputs
        , PV2.txInfoReferenceInputs = refInputs
        , PV2.txInfoFee = transCoin (txBody ^. feeTxBodyL)
        , PV2.txInfoMint = Alonzo.transMintValue (txBody ^. mintTxBodyL)
        , PV2.txInfoDCert = txCerts
        , PV2.txInfoWdrl = PV2.fromList $ Alonzo.transTxBodyWithdrawals txBody
        , PV2.txInfoValidRange = timeRange
        , PV2.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV2.txInfoRedeemers = plutusRedeemers
        , PV2.txInfoData = PV2.fromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV2.txInfoId = Alonzo.transTxBodyId txBody
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV2.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

instance Crypto c => EraPlutusTxInfo 'PlutusV3 (ConwayEra c) where
  toPlutusTxCert _ = pure . transTxCert

  toPlutusScriptPurpose = transScriptPurpose

  toPlutusTxInfo proxy pp epochInfo systemStart utxo tx = do
    timeRange <- Alonzo.transValidityInterval pp epochInfo systemStart (txBody ^. vldtTxBodyL)
    inputs <- mapM (Babbage.transTxInInfoV2 utxo) (Set.toList (txBody ^. inputsTxBodyL))
    refInputs <- mapM (Babbage.transTxInInfoV2 utxo) (Set.toList (txBody ^. referenceInputsTxBodyL))
    outputs <-
      zipWithM
        (Babbage.transTxOutV2 . TxOutFromOutput)
        [minBound ..]
        (F.toList (txBody ^. outputsTxBodyL))
    txCerts <- Alonzo.transTxBodyCerts proxy txBody
    plutusRedeemers <- Babbage.transTxRedeemers proxy tx
    pure
      PV3.TxInfo
        { PV3.txInfoInputs = inputs
        , PV3.txInfoOutputs = outputs
        , PV3.txInfoReferenceInputs = refInputs
        , PV3.txInfoFee = coinToLovelace (txBody ^. feeTxBodyL)
        , PV3.txInfoMint = Alonzo.transMultiAsset (txBody ^. mintTxBodyL)
        , PV3.txInfoTxCerts = txCerts
        , PV3.txInfoWdrl = transTxBodyWithdrawals txBody
        , PV3.txInfoValidRange = timeRange
        , PV3.txInfoSignatories = Alonzo.transTxBodyReqSignerHashes txBody
        , PV3.txInfoRedeemers = plutusRedeemers
        , PV3.txInfoData = PV3.fromList $ Alonzo.transTxWitsDatums (tx ^. witsTxL)
        , PV3.txInfoId = Alonzo.transTxBodyId txBody
        , -- FIXME: implement for plutus v3
          PV3.txInfoVotes = error "Unimplemented"
        , -- FIXME: implement for plutus v3
          PV3.txInfoProposalProcedures = error "Unimplemented"
        , -- FIXME: implement for plutus v3
          PV3.txInfoCurrentTreasuryAmount = error "Unimplemented"
        , -- FIXME: implement for plutus v3
          PV3.txInfoTreasuryDonation = error "Unimplemented"
        }
    where
      txBody = tx ^. bodyTxL

  toPlutusScriptContext proxy txInfo scriptPurpose =
    PV3.ScriptContext txInfo <$> toPlutusScriptPurpose proxy scriptPurpose

-- | Translate all `Withdrawal`s from within a `TxBody`
transTxBodyWithdrawals :: EraTxBody era => TxBody era -> PV3.Map PV3.Credential PV3.Lovelace
transTxBodyWithdrawals txBody =
  PV3.fromList $
    map (\(RewardAcnt _networkId cred, c) -> (transCred cred, coinToLovelace c)) $
      Map.toList (unWithdrawals $ txBody ^. withdrawalsTxBodyL)

transTxCert :: ConwayEraTxCert era => TxCert era -> PV3.TxCert
transTxCert = \case
  RegPoolTxCert PoolParams {ppId, ppVrf} ->
    PV3.TxCertPoolRegister (transKeyHash ppId) (PV3.PubKeyHash (PV3.toBuiltin (hashToBytes ppVrf)))
  RetirePoolTxCert poolId (EpochNo retireEpochNo) ->
    PV3.TxCertPoolRetire (transKeyHash poolId) (toInteger retireEpochNo)
  RegTxCert stakeCred ->
    PV3.TxCertRegStaking (transCred stakeCred) Nothing
  UnRegTxCert stakeCred ->
    PV3.TxCertUnRegStaking (transCred stakeCred) Nothing
  RegDepositTxCert stakeCred deposit ->
    PV3.TxCertRegStaking (transCred stakeCred) (Just (transCoin deposit))
  UnRegDepositTxCert stakeCred refund ->
    PV3.TxCertUnRegStaking (transCred stakeCred) (Just (transCoin refund))
  DelegTxCert stakeCred delegatee ->
    PV3.TxCertDelegStaking (transCred stakeCred) (transDelegatee delegatee)
  RegDepositDelegTxCert stakeCred delegatee deposit ->
    PV3.TxCertRegDeleg (transCred stakeCred) (transDelegatee delegatee) (transCoin deposit)
  AuthCommitteeHotKeyTxCert coldCred hotCred ->
    PV3.TxCertAuthHotCommittee (transColdCommitteeCred coldCred) (transHotCommitteeCred hotCred)
  ResignCommitteeColdTxCert coldCred _anchor ->
    PV3.TxCertResignColdCommittee (transColdCommitteeCred coldCred)
  RegDRepTxCert drepCred deposit _anchor ->
    PV3.TxCertRegDRep (transDRepCred drepCred) (transCoin deposit)
  UnRegDRepTxCert drepCred refund ->
    PV3.TxCertUnRegDRep (transDRepCred drepCred) (transCoin refund)
  UpdateDRepTxCert drepCred _anchor ->
    PV3.TxCertUpdateDRep (transDRepCred drepCred)

transDRepCred :: Credential 'DRepRole c -> PV3.DRepCredential
transDRepCred = PV3.DRepCredential . transCred

transColdCommitteeCred :: Credential 'ColdCommitteeRole c -> PV3.ColdCommitteeCredential
transColdCommitteeCred = PV3.ColdCommitteeCredential . transCred

transHotCommitteeCred :: Credential 'HotCommitteeRole c -> PV3.HotCommitteeCredential
transHotCommitteeCred = PV3.HotCommitteeCredential . transCred

transDelegatee :: Delegatee c -> PV3.Delegatee
transDelegatee = \case
  DelegStake poolId -> PV3.DelegStake (transKeyHash poolId)
  DelegVote drep -> PV3.DelegVote (transDRep drep)
  DelegStakeVote poolId drep -> PV3.DelegStakeVote (transKeyHash poolId) (transDRep drep)

transDRep :: DRep c -> PV3.DRep
transDRep = \case
  DRepCredential drepCred -> PV3.DRep (transDRepCred drepCred)
  DRepAlwaysAbstain -> PV3.DRepAlwaysAbstain
  DRepAlwaysNoConfidence -> PV3.DRepAlwaysNoConfidence

transScriptPurpose ::
  (EraPlutusTxInfo l era, PlutusTxCert l ~ PV3.TxCert) =>
  proxy l ->
  ScriptPurpose era ->
  Either (ContextError era) PV3.ScriptPurpose
transScriptPurpose proxy = \case
  Minting policyId -> pure $ PV3.Minting (Alonzo.transPolicyID policyId)
  Spending txIn -> pure $ PV3.Spending (transTxIn txIn)
  Rewarding (RewardAcnt _networkId cred) ->
    pure $ PV3.Rewarding (transCred cred)
  Certifying txCert -> PV3.Certifying <$> toPlutusTxCert proxy txCert

transTxCertV1V2 ::
  (ShelleyEraTxCert era, Inject (ConwayContextError era) (ContextError era)) =>
  TxCert era ->
  Either (ContextError era) PV1.DCert
transTxCertV1V2 txCert =
  case Alonzo.transTxCertCommon txCert of
    Nothing -> Left $ inject $ CertificateNotSupported txCert
    Just cert -> Right cert
